{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parse.Haskell.Lex
(
    -- * Lexical analysis

    lex
    , filterLexeme

    -- * Lexers

    , Program
    , program
    , lexeme
    , whitespace
    , qVarId
    , qVarSym
    , qConId
    , reservedId
    , comment
    , nComment
)
where

import Prelude hiding (lex)

import qualified Control.Applicative as A
import qualified Data.Char as C
import qualified Data.Maybe as N

import Parse.Monad
    (
        (<|>)
    )

import qualified Parse.Haskell.Lex.Token as T
import qualified Parse.Location as L
import qualified Parse.Monad as M
import qualified Parse.Monad.Parsec as P

-- | Perform lexical analysis on the entire string.

lex
    :: L.Path -- ^ path of the file containing the source (this is for error reporting)
    -> String -- ^ source code of a Haskell module
    -> Either M.Error [L.Located T.Token]

lex = P.lex (program <* M.end)

located arg = L.MkLocated <$> M.getLocation <*> arg

-- | Discard whitespaces. (Whitespaces include comments.)
filterLexeme :: [L.Located T.Token] -> [L.Located T.Lexeme]
filterLexeme list = [ L.MkLocated x y | L.MkLocated x (Right y) <- list ]

type Program = [L.Located T.Token]

-- | Mixture of 'lexeme's and 'whitespace's.
program :: (M.MonadLex m) => m [L.Located T.Token]
program = M.many (lexeme <|> whitespace)

lexeme :: (M.MonadLex m) => m (L.Located T.Token)
lexeme = located $ fmap Right $
    special
    <|> literal
    <|> M.try qVarId
    <|> M.try qConId
    <|> M.try qVarSym
    <|> M.try qConSym
    <|> reservedId
    <|> reservedOp

whitespace :: (M.MonadLex m) => m (L.Located T.Token)
whitespace = located $ Left <$> (whiteString <|> comment <|> nComment)
whiteString = T.White <$> M.many1 whiteChar
whiteChar = uniWhite -- newLine <|> verTab <|> space <|> tab <|> uniWhite

-- | Line comment.
comment = T.LineComment <$> (M.string dashes <++> (M.charSatisfying (not . isSymbol) <:> content))
    where
        content = M.try newLine <|> (M.anyChar <:> content)

-- | Block comment.
nComment = T.BlockComment <$> M.string "{-" -- FIXME

dashes = "--"
newLine =
    (carriageReturn <++> (N.fromMaybe "" <$> M.optional lineFeed))
    <|> lineFeed
    <|> formFeed

(<:>) = A.liftA2 (:)
(<++>) = A.liftA2 (++)

carriageReturn = M.string "\r"
lineFeed = M.string "\n"
formFeed = M.string "\f"

reserved = fmap T.Reserved . M.choice . map (M.try . M.string)

reservedId = reserved reservedIds

reservedOp = reserved reservedOps

{-
Note for reservedIds and reservedOps:

If x is a prefix of y, then x must come after y in the list.
This is because <|> is not commutative;
M.choice picks the first (not the longest) matching string.
-}

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
        , "infixl"
        , "infixr"
        , "infix"
        , "instance"
        , "in"
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
        , "::"
        , ":"
        , "=>"
        , "="
        , "\\"
        , "|"
        , "<-"
        , "->"
        , "@"
        , "~"
    ]

-- | Qualified variable identifier.
qVarId = T.QVarId <$> qualifier <*> varId

-- | Qualified constructor identifier.
qConId = T.QConId <$> qualifier <*> conId

-- | Qualified variable symbol.
qVarSym = T.QVarSym <$> qualifier <*> varSym

-- | Qualified constructor symbol.
qConSym = T.QConSym <$> qualifier <*> conSym

qualifier =
    (M.try (conId <++> M.string ".") <++> qualifier) <|> pure ""

-- | Unqualified variable identifier.
varId = do
    str <- (:) <$> small <*> M.many (small <|> large <|> digit <|> apostrophe)
    if str `elem` reservedIds
        then M.unexpected $ str ++ " is a reserved keyword"
        else return str

-- | Unqualified variable symbol.
varSym = do
    str <- (:) <$> nonColon <*> M.many symbol
    case str of
        _ | str == dashes -> M.unexpected "dashes"
        _ | str `elem` reservedOps -> M.unexpected "reservedOps"
        _ -> return str
    where
        nonColon = do
            s <- symbol
            if s == ':'
                then M.expected "non-colon"
                else return s

conSym = do
    str <- (:) <$> M.char ':' <*> M.many symbol
    case str of
        _ | str `elem` reservedOps -> M.unexpected "reservedOps"
        _ -> return str

symbol = M.named "symbol" $ M.charSatisfying isSymbol

isSymbol = (isAscSymbol <||> isUniSymbol) <&&> notF isSpecial
    where
        isAscSymbol c = c `elem` "!#$%&*+./<=>?@\\^|-~:"
        isUniSymbol c = C.isSymbol c || C.isPunctuation c

(<&&>) f g x = f x && g x
(<||>) f g x = f x || g x
notF f x = not (f x)

uniWhite = M.uniWhite

literal =
    integer
    <|> string

decimal = T.Decimal <$> M.many1 digit
integer = decimal

special = M.named "special" $ T.Special <$> M.charSatisfying isSpecial
isSpecial c = c `elem` "(),;[]`{}"

string = M.named "string" $ T.String <$> (quote <++> remaining)
    where
        singleton x = [x]
        quote = M.named "quote" $ singleton <$> M.char '"'
        remaining = M.try quote <|> (M.anyChar <:> remaining)

-- | Unqualified constructor identifier.
conId = (:) <$> large <*> M.many (small <|> large <|> digit <|> apostrophe)

-- | Lower-case character.
small = M.lower

-- | Upper-case character.
large = M.upper

-- | Decimal digit.
digit = M.digit

apostrophe = M.char '\''
