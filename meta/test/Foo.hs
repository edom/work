module Foo where

foo :: Int
foo = 0

bar :: Int -> Int
bar foo = foo

baz :: foo a -> foo a
baz = id

qux :: Int
qux = foo + 1
