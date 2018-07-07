module Meta.PegGenRule (
    -- * Grammar
    Grammar
    , grammar
    , find
    , find_start
    , get_start_name
    , set_start
    -- * Rule
    , Rule
    , get_name
    , get_exp
    , Name
    -- * Module
    , Module_Meta_PegGenRule(..)
    , module_Meta_PegGenRule
    -- * Expression
    , Exp(..)
) where

import Prelude ()
import Meta.PreludeGrammar

import qualified Meta.Map as M

{- |
A set of rules, with one starting rule.

Notes:

The programmer is responsible to ensure that the grammar is left-factored and non-left-recursive.

This is an example that is /not/ left-factored:

@
A ::= B C | B
@

In the following example, @A@ will never match @B C@ due to /ordered choice/:

@
A ::= B | B C
@

@B C | B@ and @B | B C@ differ due to ordered choice.

This /left-recursive/ example doesn't terminate:

@
A ::= A B | B
@
-}
data Grammar t
    = MkGrammar {
        _gStart :: Name
        , _gRules :: M.Map Name (Rule t)
    } deriving (Read, Show)

-- | See notes on 'Grammar'.
grammar
    :: Name -- ^ name of the starting rule
    -> [Rule t] -- ^ rules; must have unique names
    -> Grammar t

grammar name rules = MkGrammar name (M.from_list get_name rules)

get_start_name :: Grammar t -> Name
get_start_name = _gStart

set_start :: Name -> Grammar t -> Grammar t
set_start n g = g { _gStart = n }

find :: Name -> Grammar t -> Either String (Exp t)
find name gram = maybe (Left $ "Cannot find rule: " ++ name) (Right . get_exp) $ M.lookup name (_gRules gram)

find_start :: Grammar t -> Either String (Exp t)
find_start gram = find (_gStart gram) gram

type Name = String

data Rule t
    = MkRule {
        _rName :: Name
        , _rExp :: Exp t
    } deriving (Read, Show)

{- |
Usage:

Combine this with @RecordWildCards@ language extension to simulate first-class modules.

@
import qualified Meta.PegGenRule as R

foo =
    ...
    where
        -- This is similar to "open Meta.PegGenRule" in Standard ML.
        R.Module_Meta_PegGenRule{..} = R.module_Meta_PegGenRule
@
-}
data Module_Meta_PegGenRule t
    = Module_Meta_PegGenRule {
        rule :: Name -> Exp t -> Rule t
        , empty :: Exp t
        , term :: t -> Exp t
        , seq :: Exp t -> Exp t -> Exp t
        , end :: Exp t
        , call :: Name -> Exp t
        , many :: Exp t -> Exp t
        , or :: Exp t -> Exp t -> Exp t
    }

infixr 5 `seq`

module_Meta_PegGenRule :: Module_Meta_PegGenRule t
module_Meta_PegGenRule = Module_Meta_PegGenRule {
        rule = MkRule
        , empty = Empty
        , term = Term
        , seq = Seq
        , end = End
        , call = Call
        , many = Many
        , or = Or
    }

get_name :: Rule t -> Name
get_name = _rName

get_exp :: Rule t -> Exp t
get_exp = _rExp

data Exp t
    = Empty -- ^ empty (always success)
    | Fail -- ^ fail (always fail)
    | End -- ^ matches end of input
    | Term t -- ^ terminal
    | Call Name -- ^ reference to a rule
    | Seq (Exp t) (Exp t) -- ^ sequence
    | Or (Exp t) (Exp t) -- ^ ordered choice
    | Many (Exp t) -- ^ zero or more
    | And (Exp t) -- ^ positive lookahead
    | Not (Exp t) -- ^ negative lookahead
    deriving (Read, Show)
