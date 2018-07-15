{- |
Class file parsing.
-}
module Meta.JvmCls where

import Prelude ()
import Meta.Prelude

import qualified Meta.JvmAccess as A
import qualified Meta.JvmClsConst as K

-- * Constant pool

{- |
Every function @cp_get_*@ follows this pattern:

@
cp_get_/something/ :: Class_file -> PIndex -> Either String /sometype/
@
-}
cp_get :: (Monad m) => Class_file -> K.PIndex -> m K.Constant
cp_get c i = maybe (fail "invalid constant pool index") return $ (c_pool c `at` (i - 1))

-- | See 'cp_get'.
cp_get_utf8 :: Class_file -> K.PIndex -> EitherString ByteString
cp_get_utf8 c i = cp_get c i >>= K.get_utf8

-- | See 'cp_get'.
cp_get_nameandtype :: Class_file -> K.PIndex -> EitherString (Word16, Word16)
cp_get_nameandtype c i = cp_get c i >>= K.get_nameandtype

-- | See 'cp_get'.
cp_get_class :: Class_file -> K.PIndex -> EitherString Word16
cp_get_class c i = cp_get c i >>= K.get_class

-- | See 'cp_get'.
cp_get_integer :: Class_file -> K.PIndex -> EitherString Int32
cp_get_integer c i = cp_get c i >>= K.get_integer

-- * Class

{- |
This represents a parsed class file.

@c_super c@ is zero iff @c@ represents the java.lang.Object class.
-}
data Class
    = Mk_class
    {
        c_minor :: Word16 -- ^ class file minor version
        , c_major :: Word16 -- ^ class file major version
        , c_pool :: [K.Constant] -- ^ constant pool
        , c_access :: A.Access -- ^ public, protected, etc.
        , c_this :: K.PIndex -- ^ (Class) of this class
        , c_super :: K.PIndex -- ^ (Class) of the superclass (parent class) of this class
        , c_ifaces :: [K.PIndex] -- ^ interfaces implemented by this class
        , c_fields :: [K.Field_info] -- ^ fields of this class
        , c_methods :: [K.Method_info] -- ^ methods of this class
        , c_attrs :: [K.Attribute] -- ^ usually ignored
    }
    deriving (Read, Show)

-- | The representation of a class for storage on disk.
type Class_file = Class
