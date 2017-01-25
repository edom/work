module Parse.Monad
(
    -- * Parse

    MonadParse(..)

    -- * Lex

    , MonadLex(..)

    -- * Combinator

    , optional

    -- * Error

    , Error(..)

    -- * Reexports

    , M.mapM
    , M.mapM_
)
where

import qualified Control.Monad as M

import qualified Parse.Location as L

class (Monad m) => MonadParse m where

    getLocation :: m L.Location

    -- | This matches the end of input.
    end :: m ()

    -- | The expression @many x@ matches zero or more occurrences of @x@.
    many :: m a -> m [a]

    -- | The expression @many1 x@ matches one or more occurrences of @x@.
    many1 :: m a -> m [a]

    choice :: [m a] -> m a

    {- |
The expression @'try' x \<|\> y@ matches @x@ or backtracks and matches @y@.

You must use 'try' if @x@ and @y@ share a prefix.
    -}
    (<|>) :: m a -> m a -> m a
    infixr 3 <|>

    {- |
The expression @try x@ is the same as @x@,
but if @x@ fails, then @try x@ does not consume any input.
    -}
    try :: m a -> m a

    unexpected :: String -> m a

    expected :: String -> m a

    named :: String -> m a -> m a

class (MonadParse m) => MonadLex m where

    -- | This matches a character satisfying 'Data.Char.isAlphaNum'.
    alphaNum :: m Char

    -- | This matches a character satisfying 'Data.Char.isSpace'.
    uniWhite :: m Char

    lower :: m Char
    upper :: m Char
    digit :: m Char

    oneOf :: [Char] -> m Char

    anyChar :: m Char

    charSatisfying :: (Char -> Bool) -> m Char

    -- | The expression @char x@ matches the character @x@.
    char :: Char -> m Char

    -- | The expression @string x@ matches the string @x@.
    string :: String -> m String

data Error
    = MkError
    {
        location :: L.Location
        , message :: String
    }
    deriving (Read, Show)

{- |
The expression @optional x@ consumes the same input as @x@ does.
-}
optional :: (MonadParse m) => m a -> m (Maybe a)
optional x = fmap Just x <|> pure Nothing
