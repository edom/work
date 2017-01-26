{- |
The steps of parsing a Haskell module:

- Untab (replace tabs with spaces): @[Char] -> [Char]@.
- Lex (perform lexical analysis): @[Char] -> [Token]@
- Infer layout: @[Token] -> [LToken]@
- Remove layout (insert braces and semicolons): @[LToken] -> [Token]@
- Remove white tokens: @[Token] -> [Lexeme]@
- Parse: @[Lexeme] -> Module@

Note that the types are approximate.

-}
module Parse.Haskell.Example
where

import Control.Monad ((>=>))

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Parse.Haskell.Layout as J
import qualified Parse.Haskell.Lex as K
import qualified Parse.Haskell.Parse as P
import qualified Parse.Location as L
import qualified Parse.Monad as M
import qualified Parse.Monad.Parsec as N

prettyLocation loc =
    take 32 $ L.path loc ++ " " ++ show (L.line loc) ++ " " ++ show (L.column loc) ++ repeat ' '

prettyLocated (L.MkLocated loc thing) = prettyLocation loc ++ show thing

testLex = do
    src <- BC.unpack <$> B.readFile "Test.hs"
    case K.filterLexeme <$> K.lex "Test.hs" src of
        Left e -> putStrLn $ M.message e
        Right lexemes -> mapM_ (putStrLn . prettyLocated) lexemes

testParse = do
    src <- BC.unpack <$> B.readFile "Test.hs"
    case doParse "Test.hs" src of
        Left e -> putStrLn $ M.message e
        Right result -> print result
    where
        doParse path = do
            N.lex (K.program <* M.end) path
            >=> N.parse (P.module_ <* M.end) path . K.filterLexeme

testPrepare = do
    src <- BC.unpack <$> B.readFile "Test.hs"
    case N.lex (K.program <* M.end) "Test.hs" src of
        Left e -> putStrLn $ M.message e
        Right tokens -> mapM_ (putStrLn . prettyLocated) $ J.prepare tokens

testUnlayout = do
    src <- BC.unpack <$> B.readFile "Test.hs"
    case N.lex (K.program <* M.end) "Test.hs" src of
        Left e -> putStrLn $ M.message e
        Right tokens -> mapM_ (putStrLn . prettyLocated) $ J.unlayout tokens
