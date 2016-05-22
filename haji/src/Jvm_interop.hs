module Jvm_interop
where

import qualified Data.ByteString.UTF8 as Bu

import qualified Jvm_arch as A
import qualified Jvm_load as L
import qualified Jvm_execute as E
import qualified Jvm_type as T
import qualified Jvm_value as V
import qualified List as Li

-- * Calling Java code

{- |
This loads the class, finds the method in the class,
places the arguments, calls the method,
and pops the return value.

If the method is an instance method, it is your responsibility
to pass the instance as the first argument.
-}
call
    :: (A.Stateful m, E.Execute m, L.Load m)
    => Class_name -- ^ the /binary name/ (as defined by Java Language Specification) of the class containing the method
    -> T.Type -- ^ method return type
    -> Method_name -- ^ method name
    -> [T.Type] -- ^ method argument types
    -> [V.Value] -- ^ method arguments
    -> m V.Value -- ^ method return value

call cname rtype mname atypes args =
    E.call (Bu.fromString cname) (Bu.fromString mname) (T.Mk_signature atypes rtype) args

type Class_name = String
type Method_name = String
