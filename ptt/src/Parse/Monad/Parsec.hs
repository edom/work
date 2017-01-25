{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Parse.Monad.Parsec
(
    Parsec
    , lex
    , parse
    , token
)
where

import Prelude hiding (lex)

import qualified Data.Functor.Identity as I

import qualified Text.Parsec as T
import qualified Text.Parsec.Error as E
import qualified Text.Parsec.Pos as P

import qualified Parse.Location as L
import qualified Parse.Monad as M

newtype Parsec s a = In { out :: T.ParsecT s () I.Identity a }

instance Functor (Parsec s) where
    fmap f = In . fmap f . out

instance Applicative (Parsec s) where
    pure = In . pure
    (<*>) ff fx = In (out ff <*> out fx)

instance Monad (Parsec s) where
    return = pure
    fail = In . fail
    (>>=) m k = In (out m >>= out . k)

instance (T.Stream s I.Identity t, Show t) => M.MonadParse (Parsec s) where
    getLocation = In $ makeLocation <$> T.getPosition
    end = In T.eof
    many = In . T.many . out
    many1 = In . T.many1 . out
    choice = In . T.choice . map out
    (<|>) a b = In $ out a T.<|> out b
    try = In . T.try . out
    unexpected = In . T.unexpected
    expected s = In (T.parserZero T.<?> s)
    named s m = In (out m T.<?> s)

instance M.MonadLex (Parsec String) where
    char = In . T.char
    string = In . T.string
    alphaNum = In T.alphaNum
    uniWhite = In T.space
    lower = In T.lower
    upper = In T.upper
    digit = In T.digit
    oneOf = In . T.oneOf
    anyChar = In T.anyChar
    charSatisfying = In . T.satisfy

makeLocation :: T.SourcePos -> L.Location
makeLocation p = L.MkLocation (T.sourceName p) (T.sourceLine p) (T.sourceColumn p)

lex :: Parsec String a -> L.Path -> String -> Either M.Error a
lex grammar path code =
    case T.parse (out grammar) path code of
        Left e -> Left $ translateError e
        Right x -> Right x

parse :: (T.Stream s I.Identity t) => Parsec s a -> L.Path -> s -> Either M.Error a
parse grammar path code =
    case T.parse (out grammar) path code of
        Left e -> Left $ translateError e
        Right x -> Right x

translateError :: T.ParseError -> M.Error
translateError e = M.MkError loc (unlines $ map msg $ E.errorMessages e)
    where
        loc = makeLocation (T.errorPos e)
        msg m = case m of
            E.SysUnExpect s -> "Unexpected: " ++ s
            E.UnExpect s -> "Unexpected: " ++ s
            E.Expect s -> "Expecting: " ++ s
            E.Message s -> s
            _ -> E.messageString m

token :: (T.Stream s I.Identity t) => (t -> String) -> (t -> L.Location) -> (t -> Maybe a) -> Parsec s a
token showTok locFromTok testTok = In $ T.token showTok posFromTok testTok
    where
        posFromTok t = P.newPos path line column
            where
                L.MkLocation path line column = locFromTok t
