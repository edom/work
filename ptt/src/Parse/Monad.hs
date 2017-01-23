module Parse.Monad
(
    MonadParse(..)
)
where

import qualified Parse.Location as L

class (Monad m) => MonadParse m where
    location :: m L.Location
    many :: m a
