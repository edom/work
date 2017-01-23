{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parse.Haskell.Lex
(
    -- * Type
    Located(..)
    , locate
    , Token
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

data Located a
    = MkLocated L.Location a
    deriving (Read, Show)

locate :: Located a -> L.Location
locate (MkLocated x _) = x

located arg = MkLocated <$> M.getLocation <*> arg

instance Functor Located where
    fmap f (MkLocated a b) = MkLocated a (f b)

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

filterLexeme :: [Located Token] -> [Located Lexeme]
filterLexeme list = [ MkLocated x y | MkLocated x (Right y) <- list ]

type Program = [Located Token]

-- | This consumes the program until the end of input.
program :: (M.MonadLex m) => m [Located Token]
program = M.many (lexeme <|> whitespace) <* M.end

lexeme :: (M.MonadLex m) => m (Located Token)
lexeme = located $ fmap Right $
    M.try qVarId
    <|> reservedId
    <|> reservedOp
    <|> qConId

whitespace :: (M.MonadLex m) => m (Located Token)
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
