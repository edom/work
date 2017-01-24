{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parse.Haskell.Lex
(
    -- * Type
    Token
    , Lexeme(..)
    , Whitespace(..)
    , filterLexeme
    -- * Program
    , Program
    , program
    -- * Identifiers
    , qVarId
    , qVarSym
    , qConId
    , reservedId
    -- * Layout
    , unlayout
)
where

import qualified Control.Applicative as A
import qualified Data.Char as C
import qualified Data.Maybe as N

import Parse.Monad
    (
        (<|>)
    )

import qualified Parse.Location as L
import qualified Parse.Monad as M

located arg = L.MkLocated <$> M.getLocation <*> arg

type Token = Either Whitespace Lexeme

data Lexeme
    = QVarId String String
    | QConId String String
    | QVarSym String String
    | Reserved String
    deriving (Read, Show)

data Whitespace
    = White String
    deriving (Read, Show)

filterLexeme :: [L.Located Token] -> [L.Located Lexeme]
filterLexeme list = [ L.MkLocated x y | L.MkLocated x (Right y) <- list ]

type Program = [L.Located Token]

-- | This consumes the program until the end of input.
program :: (M.MonadLex m) => m [L.Located Token]
program = M.many (lexeme <|> whitespace) <* M.end

lexeme :: (M.MonadLex m) => m (L.Located Token)
lexeme = located $ fmap Right $
    M.try qVarId
    <|> reservedId
    <|> reservedOp
    <|> qConId

whitespace :: (M.MonadLex m) => m (L.Located Token)
whitespace = located $ Left . White . concat <$> M.many1 whiteStuff
whiteStuff = whiteChar <|> comment <|> nComment
whiteChar = singleton <$> uniWhite -- newLine <|> verTab <|> space <|> tab <|> uniWhite

-- | Line comment.
comment = dashes >> newLine

-- | Block comment.
nComment = M.string "{-" -- FIXME

dashes = M.string "--"
newLine =
    (carriageReturn <++> (N.fromMaybe "" <$> M.optional lineFeed))
    <|> lineFeed
    <|> formFeed

singleton x = [x]

(<++>) = A.liftA2 (++)

carriageReturn = M.string "\r"
lineFeed = M.string "\n"
formFeed = M.string "\f"


reserved = fmap Reserved . M.choice . map (M.try . M.string)

reservedId = reserved reservedIds

reservedOp = reserved reservedOps

reservedIds = [
        "case"
        , "class"
        , "data"
        , "default"
        , "deriving"
        , "do"
        , "else"
        , "foreign"
        , "if"
        , "import"
        , "in"
        , "infix"
        , "infixl"
        , "infixr"
        , "instance"
        , "let"
        , "module"
        , "newtype"
        , "of"
        , "then"
        , "type"
        , "where"
        , "_"
    ]

reservedOps = [
        ".."
        , ":"
        , "::"
        , "="
        , "\\"
        , "|"
        , "<-"
        , "->"
        , "@"
        , "~"
        , "=>"
    ]

-- | Qualified variable identifier.
qVarId = QVarId <$> qualifier <*> varId

-- | Qualified constructor identifier.
qConId = QConId <$> qualifier <*> conId

-- | Qualified variable symbol.
qVarSym = QVarSym <$> qualifier <*> varSym

qualifier =
    (M.try (conId <++> M.string ".") <++> qualifier) <|> pure ""

-- | Unqualified variable identifier.
varId = do
    str <- (:) <$> small <*> M.many (small <|> large <|> digit <|> apostrophe)
    if str `elem` reservedIds
        then M.unexpected $ str ++ " is a reserved keyword"
        else return str

-- | Unqualified variable symbol.
varSym = (:) <$> symbol <*> M.many (symbol <|> M.char ':') -- TODO minus (reservedOp | dashes)

symbol = ascSymbol <|> uniSymbol
ascSymbol = M.oneOf "!#$%&*+./<=>?@\\^|-~"
uniSymbol = M.charSatisfying (\ c -> C.isSymbol c || C.isPunctuation c)
uniWhite = M.uniWhite

-- | Unqualified constructor identifier.
conId = (:) <$> large <*> M.many (small <|> large <|> digit <|> apostrophe)

-- | Lower-case character.
small = M.lower

-- | Upper-case character.
large = M.upper

-- | Decimal digit.
digit = M.digit

apostrophe = M.char '\''

{- |
Insert braces and semicolons implied by indentations.

This should be idempotent (@unlayout . unlayout = unlayout@).

See <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3 Layout> (Haskell 2010 Report).
-}
unlayout :: [L.Located Token] -> [L.Located Token]
unlayout x = x -- TODO
