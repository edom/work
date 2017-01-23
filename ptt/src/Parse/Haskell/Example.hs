module Parse.Haskell.Example
where

import Control.Monad ((>=>))

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Parse.Haskell.Lex as K
import qualified Parse.Haskell.Parse as P
import qualified Parse.Monad as M
import qualified Parse.Monad.Parsec as N

testLex = do
    src <- BC.unpack <$> B.readFile "Test.hs"
    case K.filterLexeme <$> N.lex K.program "Test.hs" src of
        Left e -> putStrLn $ M.message e
        Right lexemes -> mapM_ print lexemes

testParse = do
    src <- BC.unpack <$> B.readFile "Test.hs"
    case doParse "Test.hs" src of
        Left e -> putStrLn $ M.message e
        Right result -> print result
    where
        doParse path = do
            N.lex K.program path
            >=> N.parse P.module_ path . K.filterLexeme
