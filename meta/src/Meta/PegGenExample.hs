{-# LANGUAGE RecordWildCards #-}

module Meta.PegGenExample where

import Prelude (IO, putStrLn)
import Meta.PreludeGrammar

import Meta.PegGen

example_grammar :: Grammar Char
example_grammar = grammar "Start" [
        rule "Start" $ call "Munch A" `seq` call "Munch B or C"
        , rule "Munch A" $ many (term 'A')
        -- , rule "Munch B or C" $ many (term 'B' `or` term 'C')
        , rule "Munch B or C" $ many (term 'B' `or` (term 'B' `seq` term 'C'))
    ]
    where
        Module_Meta_PegGenRule{..} = module_Meta_PegGenRule

example_parse :: IO ()
example_parse = case parse example_grammar "AABCBC" of
    Left msg -> putStrLn msg
    Right (tree, _, rest) -> do
        putStrLn $ "Unparsed input: " ++ show rest
        putStrLn $ "Syntax tree:\n" ++ pretty tree
