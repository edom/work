module Meta.Tys where

-- * Symbol

type ModName = String

type Name = String

data Sym
    = MkSym {
        sMod :: ModName
        , sName :: Name
    } deriving (Read, Show)

-- * Type

data Type
    = Any -- ^ any
    | Num -- ^ number
    | Str -- ^ string
    | Nul -- ^ null
    | Und -- ^ undefined
    | Cls Sym -- ^ a class or an interface
    | Arr Type -- ^ type[]
    | Fun [Type] Type -- ^ (a1, a2, ...) => b
    | And Type Type -- ^ a & b
    | Or Type Type -- ^ a | b
    {- lambda calculus -}
    | App Type [Type] -- ^ a\<b1, b2, ...\>
    deriving (Read, Show)
