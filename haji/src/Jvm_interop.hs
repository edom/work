module Jvm_interop
where

import qualified Data.ByteString.UTF8 as Bu

import qualified Jvm_arch as A
import qualified Jvm_state as S
import qualified Jvm_type as T
import qualified Jvm_value as V

-- * Marshalling values from Haskell to Java

-- * Marshalling values from Java to Haskell

-- * Calling Java code from Haskell

{- |
This marshals the arguments, calls the method, and unmarshals the return value.
-}
call_static
    :: Class_name
    -> T.Type -- ^ return type
    -> Method_name
    -> [T.Type] -- ^ argument types
    -> [V.Value] -- ^ arguments
    -> A.S V.Value -- ^ return value

call_static cname rtype mname atypes args = do
    cls <- S.load_class (Bu.fromString cname)
    met <- S.get_method cls (Bu.fromString mname) (T.Mk_signature atypes rtype)
    S.enter_static cls met args
    S.step_until_returned

type Class_name = String
type Method_name = String

-- * Calling Haskell code from Java

-- * Implementing Java interfaces in Haskell
