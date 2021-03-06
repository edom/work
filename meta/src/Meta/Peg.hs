{- |
Consider using "Meta.PegGen" instead of this.

See also:

    * <http://hackage.haskell.org/package/frisby>

    * <https://en.wikipedia.org/wiki/Parsing_expression_grammar>
-}
module Meta.Peg (
    -- * Expression
    Exp(..)
    -- * Rule
    , Rule(..)
    , empty
    , fail
    , append
    , concat
    , term
    , or
    , opt
    , many0
    , many1
    , and
    , not
    , (/)
    , (>)
    , (>!)
    , (>?)
    , string
    , any
    , space
    , white
    -- * Combinators
    , Value(..)
    , parse
    -- * Parse
) where

import Prelude ()
import Meta.PreludeGrammar

data Exp t a
    = Empty -- ^ empty (always success)
    | Fail -- ^ fail (always fail)
    | Term t -- ^ terminal
    | Seq a a -- ^ sequence
    | Or a a -- ^ ordered choice
    | Many a -- ^ zero or more
    | And a -- ^ positive lookahead
    | Not a -- ^ negative lookahead
    deriving (Read, Show)

instance Functor (Exp t) where
    fmap _ Empty = Empty
    fmap _ Fail = Fail
    fmap _ (Term t) = Term t
    fmap f (Seq a b) = Seq (f a) (f b)
    fmap f (Or a b) = Or (f a) (f b)
    fmap f (Many a) = Many (f a)
    fmap f (And a) = And (f a)
    fmap f (Not a) = Not (f a)

mapTerm :: (t -> u) -> Exp t a -> Exp u a
mapTerm f e = case e of
    Empty -> Empty
    Fail -> Fail
    Term a -> Term (f a)
    Seq a b -> Seq a b
    Or a b -> Or a b
    Many a -> Many a
    And a -> And a
    Not a -> Not a

data Rule t
    = RExp { rOut :: Exp t (Rule t) }
    deriving (Read, Show)

-- Is this a lawful instance?
instance Functor Rule where
    fmap f (RExp e) = RExp (mapTerm f $ fmap (fmap f) e)

empty :: Rule t
empty = RExp Empty

fail :: Rule t
fail = RExp Fail

append :: Rule t -> Rule t -> Rule t
append a b = RExp (Seq a b)

concat :: [Rule t] -> Rule t
concat [] = empty
concat (h : t) = h `append` concat t

term :: t -> Rule t
term = RExp . Term

or :: Rule t -> Rule t -> Rule t
or a b = RExp (Or a b)

opt :: Rule t -> Rule t
opt r = or r empty

many0 :: Rule t -> Rule t
many0 r = RExp (Many r)

many1 :: Rule t -> Rule t
many1 r = append r (many0 r)

and :: Rule t -> Rule t
and = RExp . And

not :: Rule t -> Rule t
not = RExp . Not

(/) :: Rule t -> Rule t -> Rule t
(/) = or
infixr 1 /

-- sequence
(>) :: Rule t -> Rule t -> Rule t
(>) = append
infixr 1 >

-- sequence with mandatory whitespace between
(>!) :: Rule Char -> Rule Char -> Rule Char
(>!) a b = a > white > b
infixr 1 >!

-- sequence with optional whitespace between
(>?) :: Rule Char -> Rule Char -> Rule Char
(>?) a b = a > opt white > b
infixr 1 >?

string :: String -> Rule Char
string s = concat $ map term s

any :: [Rule t] -> Rule t
any [] = fail
any (h : t) = h / any t

space :: Rule Char
space = any $ term <$> " \t\n\r"

white :: Rule Char
white = many1 space

data Value t
    = No
    | Ok [t] [t]
    deriving (Read, Show)

parse :: (Eq t) => Rule t -> [t] -> Value t
parse = go . rOut
    where
        go Empty x = Ok [] x
        go Fail _ = No
        go (Term t) (h : j) | t == h = Ok [h] j
        go (Term _) _ = No
        go (Seq a b) x = case parse a x of
            Ok ha ta -> case parse b ta of
                Ok hb tb -> Ok (ha ++ hb) tb
                _ -> No
            _ -> No
        go (Or a b) x = case parse a x of
            Ok ha ta -> Ok ha ta
            _ -> parse b x
        go (Many r) x = case parse r x of
            Ok h t ->
                let Ok h1 t1 = go (Many r) t
                in Ok (h ++ h1) t1
            _ -> Ok [] x
        go (And r) x = case parse r x of
            Ok _ _ -> Ok [] x
            No -> No
        go (Not r) x = case parse r x of
            Ok _ _ -> No
            No -> Ok [] x
