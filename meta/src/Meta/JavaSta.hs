module Meta.JavaSta where

import qualified Meta.JavaExp as E
import qualified Meta.JavaType as JT

type Name = E.Name

type Exp = E.Exp Sta JT.Type ()

type Type = JT.Type

data Catch = MkCatch {
        _catch_type :: Type
        , _catch_name :: Name
        , _catch_body :: [Sta]
    } deriving (Read, Show)

type Finally = Maybe [Sta]

{- |
Do not use these data type constructors directly. Use constructor functions.
-}
data Sta
    = SExp Exp
    | SRet (Maybe Exp)
    | SThrow Exp
    | SBlock [Sta]
    | SIf Exp [Sta] (Maybe [Sta]) -- ^ conditional statement: condition, true part, false part
    | SSwitch Exp [(Exp, [Sta])] -- ^ switch expression
    | SBreak
    | SDecl JT.Type Name (Maybe Exp)
    | STry [Sta] [Catch] Finally -- ^ try-catch-finally
    | SWith [Sta] [Sta] [Catch] Finally -- ^ try-with-resources: definitions, body, catches
    | SWhile Exp [Sta]
    | SFor Sta Exp Exp [Sta]
    deriving (Read, Show)

data Case
    = CCase Exp [Sta]
    | CDefault [Sta]
    deriving (Read, Show)
