-- | Abstract syntax tree.

module Parse.Haskell.Abstract
(
    Module(..)
    , Export
    , Import(..)
    , Declaration(..)
    , Lhs(..)
    , Rhs(..)
    , Guard(..)
)
where

data Module
    = MkModule
    {
        mName :: String
        , exports :: Maybe [Export]
        , imports :: [Import]
        , declarations :: [Declaration]
    }
    deriving (Read, Show)

type Export = String

data Import
    = MkImport
    {
        qualified :: Bool
        , module_ :: String
        , alias :: String
    }
    deriving (Read, Show)

data Declaration
    = Type String String
    | Data String
    | Newtype String
    | Class String
    | Instance
    | Default
    | Foreign
    | Signature -- ^ type signature
    | Fixity -- ^ fixity declaration
    | Equation Lhs [Rhs]
    deriving (Read, Show)

data Lhs
    = Pattern
    | FunLhs
    deriving (Read, Show)

data Rhs
    = MkRhs [Guard] Exp
    deriving (Read, Show)

data Guard
    = MkGuard
    deriving (Read, Show)

data Exp
    = Lit Literal
    | EVar String
    | App Exp Exp
    | Lam Pattern Exp
    deriving (Read, Show)

data Pattern
    = PVar String
    | PCon String Pattern
    deriving (Read, Show)

data Literal
    = Str String
    deriving (Read, Show)
