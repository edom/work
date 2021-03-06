module Parse.Haskell.Lex.Token
(
    -- * Type

    Token(..)
    , Lexeme(..)
    , Whitespace(..)

    -- * Match

    , asLexeme
    , asWhitespace
)
where

import qualified Control.Applicative as A

data Token
    = TWhite Whitespace
    | TLexeme Lexeme
    deriving (Read, Show)

data Lexeme
    = QVarId String String
    | QConId String String
    | QVarSym String String
    | QConSym String String
    | Reserved String
    | Special Char
    | Decimal String
    | String String
    deriving (Read, Show, Eq)

data Whitespace
    = White String -- ^ maximal contiguous whitespace characters
    | LineComment String -- ^ including the @--@
    | BlockComment String -- ^ including the @{-@ and @-}@
    deriving (Read, Show, Eq)

asLexeme :: (A.Alternative f) => Token -> f Lexeme
asLexeme (TLexeme x) = pure x
asLexeme _ = A.empty

asWhitespace :: (A.Alternative f) => Token -> f Whitespace
asWhitespace (TWhite x) = pure x
asWhitespace _ = A.empty
