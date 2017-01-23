module Parse.Monad.Parsec
(
    Parsec
)
where

import qualified Data.Functor.Identity as I

import qualified Text.Parsec as T

import qualified Parse.Location as L
import qualified Parse.Monad as M

newtype Parsec a = In { out :: T.ParsecT String () I.Identity a }

instance Functor Parsec where
    fmap f = In . fmap f . out

instance Applicative Parsec where
    pure = In . pure
    (<*>) ff fx = In (out ff <*> out fx)

instance Monad Parsec where
    return = pure
    (>>=) m k = In (out m >>= out . k)

instance M.MonadParse Parsec where
    getLocation = In $ makeLocation <$> T.getPosition
    char = In . T.char
    string = In . T.string
    eof = In T.eof
    many = In . T.many . out
    (<|>) a b = In $ out a T.<|> out b

makeLocation :: T.SourcePos -> L.Location
makeLocation p = L.MkLocation (T.sourceName p) (T.sourceLine p) (T.sourceColumn p)
