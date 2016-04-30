{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
This module has its own /internal exception type/ that it hides from you.
All "Control.Exception"-like functions here only work with that type;
'try' only catches exceptions of that type;
'throw' only throws exceptions of that type.
-}
module Error where

import qualified Control.Exception as Ex
import qualified Control.Monad as M

import qualified Control.Error as E

import qualified Error_internal as Ei

-- * Catching

{- |
This is a specialization of 'Ex.try' in "Control.Exception"
that only catches this module's internal exception type.
-}
try :: IO a -> IO (Either String a)
try = fmap (either (Left . Ei.un_exc) Right) . Ex.try

{- |
This is like 'Ex.handle' but this uses 'Ex.try'
instead of 'Ex.catch', and
this only catches exceptions whose type is
this module's internal exception type.
-}
handle_try :: (String -> IO a) -> IO a -> IO a
handle_try h = try M.>=> either h return

{- |
This is like 'handle_try' but this ignores return values.
-}
handle_try_ :: (String -> IO a) -> IO b -> IO ()
handle_try_ h = try M.>=> either (M.void . h) (M.void . return)

-- * Throwing

{- |
This uses 'Ex.throwIO' (not 'Ex.throw') from "Control.Exception".

This throws an exception whose type is this module's internal exception type.
-}
throw :: String -> IO a
throw = Ex.throwIO . Ei.Mk_exc

-- * Rethrowing

{- |
Modify the error message.

If @x@ throws error message @e@,
then @map_error f x@ throws error message @f e@.
-}
map_error :: (String -> String) -> IO a -> IO a
map_error f = try M.>=> either (throw . f) return

{- |
Prepend a string to every error message thrown by the given computation.

@
prepend str = 'map_error' (str '++')
@

You can get something similar to a stack trace.
For example, this should print @a: b: c: message@

@
main = 'handle_try_' 'putStrLn' a
a = prepend \"a: \" b
b = prepend \"b: \" c
c = prepend \"c: \" '$' 'throw' \"message\"
@

You can also show some arguments to help you debug the program later.

@
e_div x y =
    prepend (\"e_div \" '++' 'show' x '++' 'show' y) '$' f x y
    where
        f x 0 = 'throw' \"division by zero\"
        f x y = 'div' x y
@
-}
prepend :: String -> IO a -> IO a
prepend str = map_error (str ++)

{- |
Convert certain exceptions thrown by the computation
to this module's internal exception type.

For example,
when working with the @http-conduit@ package,
you may want to use this:

@
gulp (\\ e -> 'show' (e :: HttpException)) computation
@

If the computation throws several types of exception,
you will need many @gulp@s.
You need one for each type of exception
that the computation can throw.

@
gulp (\\ e -> 'show' (e :: Exception0))
'$' gulp (\\ e -> 'show' (e :: Exception1))
'$' ...
'$' gulp (\\ e -> 'show' (e :: ExceptionM))
'$' computation
@

The code is slightly unsightly but not much can be done about it
because we cannot pass a type as an argument
to a value-level function in Haskell.
(We can do that in Template Haskell though.)
-}
gulp :: (Ex.Exception e) => (e -> String) -> IO a -> IO a
gulp format = Ex.try M.>=> either (throw . format) return

-- * Interoperation

{- $
This is for working with other failure-indicating
and error-handling mechanisms.
-}

{- |
The 'transform' function converts between implicit
and explicit style of indicating failures
by inserting 'try' and 'throw' as appropriate.
Remember that this function only works
with this module's internal exception type.

Example of implicit style (type does not have error type):

@
'IO' a
@

Example of explicit style (type has error type):

@
'IO' ('Either' 'String' a)

'E.ExceptT' 'String' 'IO' a
@
-}
class Transform a b where
    transform :: a -> b

-- explicit to implicit

-- Either e a -> IO a

-- | overlappable
instance {-# OVERLAPPABLE #-} (Show e) => Transform (Either e a) (IO a) where
    transform = either (throw . show) return

instance Transform (Either String a) (IO a) where
    transform = either throw return

-- IO (Either e a) -> IO a

-- | overlappable
instance {-# OVERLAPPABLE #-} (Show e) => Transform (IO (Either e a)) (IO a) where
    transform = M.join . fmap (either (throw . show) return)

instance Transform (IO (Either String a)) (IO a) where
    transform = M.join . fmap (either throw return)

-- ExceptT e IO a -> IO a

instance {-# OVERLAPPABLE #-} (Show e) => Transform (E.ExceptT e IO a) (IO a) where
    transform = E.runExceptT M.>=> either (throw . show) return

instance Transform (E.ExceptT String IO a) (IO a) where
    transform = E.runExceptT M.>=> either throw return

-- implicit to explicit

-- IO a -> IO (Either String a)

instance Transform (IO a) (IO (Either String a)) where
    transform = try

-- IO a -> ExceptT String IO a

instance Transform (IO a) (E.ExceptT String IO a) where
    transform = E.ExceptT . try
