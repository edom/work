{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parse.Haskell.Lex
(
    filterLexeme
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

import qualified Parse.Haskell.Token as T
import qualified Parse.Location as L
import qualified Parse.Monad as M

located arg = L.MkLocated <$> M.getLocation <*> arg

filterLexeme :: [L.Located T.Token] -> [L.Located T.Lexeme]
filterLexeme list = [ L.MkLocated x y | L.MkLocated x (Right y) <- list ]

type Program = [L.Located T.Token]

-- | This consumes the program until the end of input.
program :: (M.MonadLex m) => m [L.Located T.Token]
program = M.many (lexeme <|> whitespace) <* M.end

lexeme :: (M.MonadLex m) => m (L.Located T.Token)
lexeme = located $ fmap Right $
    M.try qVarId
    <|> reservedId
    <|> reservedOp
    <|> qConId

whitespace :: (M.MonadLex m) => m (L.Located T.Token)
whitespace = located $ Left <$> (whiteString <|> comment <|> nComment)
whiteString = T.White <$> M.many1 whiteChar
whiteChar = uniWhite -- newLine <|> verTab <|> space <|> tab <|> uniWhite

-- | Line comment.
comment = T.LineComment <$> (dashes <++> newLine)

-- | Block comment.
nComment = T.BlockComment <$> M.string "{-" -- FIXME

dashes = M.string "--"
newLine =
    (carriageReturn <++> (N.fromMaybe "" <$> M.optional lineFeed))
    <|> lineFeed
    <|> formFeed

(<++>) = A.liftA2 (++)

carriageReturn = M.string "\r"
lineFeed = M.string "\n"
formFeed = M.string "\f"

reserved = fmap T.Reserved . M.choice . map (M.try . M.string)

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
qVarId = T.QVarId <$> qualifier <*> varId

-- | Qualified constructor identifier.
qConId = T.QConId <$> qualifier <*> conId

-- | Qualified variable symbol.
qVarSym = T.QVarSym <$> qualifier <*> varSym

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
