module Parse.Haskell.Token
(
    -- * Type
    Token
    , Lexeme(..)
    , Whitespace(..)
)
where

type Token = Either Whitespace Lexeme

data Lexeme
    = QVarId String String
    | QConId String String
    | QVarSym String String
    | QConSym String String
    | Reserved String
    | Special Char
    | Decimal String
    deriving (Read, Show, Eq)

data Whitespace
    = White String -- ^ maximal contiguous whitespace characters
    | LineComment String -- ^ including the @--@
    | BlockComment String -- ^ including the @{-@ and @-}@
    deriving (Read, Show, Eq)
