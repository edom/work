module Parse.Monad
(
    MonadParse(..)
)
where

import qualified Parse.Location as L

class (Monad m) => MonadParse m where

    getLocation :: m L.Location

    char :: Char -> m Char
    string :: String -> m String

    eof :: m ()
    many :: m a -> m [a]
    (<|>) :: m a -> m a -> m a
