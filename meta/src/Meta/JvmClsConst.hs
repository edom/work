{- |
Class file constant pool.
-}
module Meta.JvmClsConst where

import Prelude ()
import Meta.Prelude

import qualified Meta.JvmAccess as A
import qualified Meta.JvmClsAtr as At

-- * Constant pool entry

-- | See 'A.PIndex'.
type PIndex = A.PIndex

-- | See 'At.Attribute'.
type Attribute = At.Attribute A.PIndex ByteString

{- |
An entry in a constant pool.

Avoid using the data constructors directly.
Use getters and function constructors instead.
-}
data Constant
    = P_utf8 ByteString
    | P_integer Int32
    | P_float Float
    | P_long Int64
    | P_double Double
    | P_class A.PIndex -- ^ must point to a 'P_utf8'
    | P_string A.PIndex
    | P_fieldref A.PIndex A.PIndex
    | P_methodref A.PIndex A.PIndex
    | P_interfacemethodref A.PIndex A.PIndex
    | P_nameandtype A.PIndex A.PIndex -- ^ part of field or method info; parameters are 'P_utf8' and 'P_class'
    | P_unused -- ^ second slot of 8-byte constant
    deriving (Read, Show)

utf8 :: ByteString -> Constant
utf8 = P_utf8

integer :: Int32 -> Constant
integer = P_integer

float :: Float -> Constant
float = P_float

long :: Int64 -> Constant
long = P_long

double :: Double -> Constant
double = P_double

class_ :: A.PIndex -> Constant
class_ = P_class

string :: A.PIndex -> Constant
string = P_string

fieldref :: A.PIndex -> A.PIndex -> Constant
fieldref = P_fieldref

methodref :: A.PIndex -> A.PIndex -> Constant
methodref = P_methodref

interfacemethodref :: A.PIndex -> A.PIndex -> Constant
interfacemethodref = P_interfacemethodref

nameandtype :: A.PIndex -> A.PIndex -> Constant
nameandtype = P_nameandtype

unused :: Constant
unused = P_unused

-- * Getters for case analysis

get_utf8 :: (Monad m) => Constant -> m ByteString
get_utf8 (P_utf8 x) = return x
get_utf8 x = fail $ "Not an Utf8: " ++ show x

get_nameandtype :: (Monad m) => Constant -> m (A.PIndex, A.PIndex)
get_nameandtype (P_nameandtype a b) = return (a, b)
get_nameandtype x = fail $ "Not a Nameandtype: " ++ show x

get_class :: (Monad m) => Constant -> m A.PIndex
get_class (P_class x) = return x
get_class x = fail $ "Not a Class: " ++ show x

get_integer :: (Monad m) => Constant -> m Int32
get_integer (P_integer x) = return x
get_integer x = fail $ "Not an Integer: " ++ show x

-- | Field.
data Field_info
    = Mk_field_info
    {
        fi_access :: A.Access
        , fi_name :: A.PIndex
        , fi_descriptor :: A.PIndex
        , fi_attributes :: [Attribute]
    }
    deriving (Read, Show)

instance A.Get_access Field_info A.Access where
    get_access = fi_access

-- | Method.
data Method_info
    = Mk_method_info
    {
        mi_access :: A.Access
        , mi_name :: A.PIndex
        , mi_descriptor :: A.PIndex
        , mi_attributes :: [Attribute]
    }
    deriving (Read, Show)

instance A.Get_access Method_info A.Access where
    get_access = mi_access

-- * Method body

-- ** Code

{- |
Method body.
-}
data Code
    = Mk_code
    {
        cd_max_stack :: Word16
        , cd_max_local :: Word16
        , cd_code :: ByteString
        , cd_handlers :: [Handler]
        , cd_attributes :: [Attribute]
    }
    deriving (Read, Show, Eq)

instance A.Get_code Code ByteString where
    get_code = cd_code

instance A.Get_attributes Code [Attribute] where
    get_attributes = cd_attributes

-- ** Exception handler

-- | Exception handler.
data Handler
    = Mk_handler
    {
        h_start_pc :: A.Pc
        , h_end_pc :: A.Pc
        , h_handler_pc :: A.Pc
        , h_catch_type :: Word16
    }
    deriving (Read, Show, Eq)
