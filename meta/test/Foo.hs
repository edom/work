module Foo where

foo :: Int
foo = 0

bar :: Int -> Int
bar foo = foo

baz :: foo a -> foo a
baz = id

qux :: Int
qux = foo + 1

f :: String -> String
f x = 'a' : x

{- |
We want the compiler to rewrite @f getLine@ to @f '<$>' getLine@.
-}
h :: IO String
h = f getLine
