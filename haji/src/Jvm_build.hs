{- |
Convenience module.

This module provides a convenient way of defining classes.
-}
module Jvm_build
where

import qualified Data.ByteString.UTF8 as Bu

import qualified Control.Monad.Trans.State as Ms

import Data.Bits
    (
        (.|.)
    )
import Data.Word
    (
        Word16
    )
import Jvm_arch
    (
        Class(..)
        , Method(..)
        , J
        , S
    )
import qualified Jvm_arch as A
import qualified Jvm_state as S
import qualified Jvm_type as T
import qualified Jvm_value as V

-- * Building a Class

type Build s a = Ms.State s a

class_ :: Class_name -> Build Class a -> Class
class_ name build =
    Ms.execState build $ Mk_class
        {
            c_name = Bu.fromString name
            , c_super = Nothing
            , c_fields = []
            , c_methods = []
            , c_pool = []
            , c_static = []
            , c_initialized = False
        }

-- * Adding Java @native@ methods implemented in Haskell

-- | Add a public static native method.
native :: Access -> T.Return_type -> Method_name -> [T.Type] -> S V.Value -> Build Class ()
native access rtype name atypes body =
    add_method $ A.Mk_method
        (or_mask access .|. b_static .|. b_native)
        (Bu.fromString name)
        (T.Mk_signature atypes rtype)
        (A.Native body)

native_io :: Access -> T.Return_type -> Method_name -> [T.Type] -> J V.Value -> Build Class ()
native_io access rtype name atypes body =
    add_method $ A.Mk_method
        (or_mask access .|. b_static .|. b_native)
        (Bu.fromString name)
        (T.Mk_signature atypes rtype)
        (A.Native_io body)

-- * XXX don't know where to place these; see also "Jvm_state"

b_public :: Word16
b_public = 0x0001

b_static :: Word16
b_static = 0x0008

b_native :: Word16
b_native = 0x0100

or_mask :: Access -> Word16
or_mask a = case a of
    Package -> 0x0000
    Public -> 0x0001
    Private -> 0x0002
    Protected -> 0x0004

type Class_name = String
type Method_name = String

-- * Bootstrap class

class Bootstrap m where
    bootstrap :: m V.Value -> Class

instance Bootstrap S where
    bootstrap main =
        class_ "<bootstrap>" $ do
            native Public T.Void "<main>" [] main

instance Bootstrap J where
    bootstrap main =
        class_ "<bootstrap>" $ do
            native_io Public T.Void "<main>" [] main

-- * Access

data Access
    = Public
    | Private
    | Protected
    | Package
    deriving (Read, Show, Eq)

-- * Internals

-- | Add method.
add_method :: Method -> Build Class ()
add_method m = Ms.modify $ \ c -> c { c_methods = m : c_methods c }
