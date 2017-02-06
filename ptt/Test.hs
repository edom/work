{-# LANGUAGE MagicHash #-}

module Test.Lex.And.Parse where

import Foo as Bar
import Module.One
import ModuleOne
import qualified Module.Two
import qualified ModuleTwo

decl0 = decl1

x = 0

y = let z = 0 in z

a = 123 + 456 -- 789

(+++) x _ = x
infixr 1 +++

f x =
    let
    {
        y = x;
    }
    in
    y

g (x, y) = (x +) y

myString = "myString"

returnUnit :: (Monad m) => m ()
returnUnit = return ()

main = int2Double# 5 +## int2Double# 6
