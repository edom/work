{-# LANGUAGE TypeFamilies #-}

{- |
Constant pool.

The usage of indexed type families here was inspired by the GHC codebase.
-}
module Meta.JvmConstPool where

import Prelude ()
import Meta.Prelude

-- * Phases and type families

{- |
This represents a parsed class file.

The parameter @r@ can be 'Raw' or 'Resolved'.
-}
type family Class r where
    Class Raw = Class_ Index Index Index
    Class Resolved = Class_ Ubs Ubs (Maybe Ubs)

-- | Constant pool entry.
type family Entry r where
    Entry Raw = Entry_ Index Index
    Entry Resolved = Entry_ Ubs Ubs

data Raw = MkRaw deriving (Read, Show)
data Resolved = MkResolved deriving (Read, Show)

-- * Other things

{- |
See 'Class' for the type parameters.

If @c :: 'Class' 'Raw'@, then @'_super' c@ is zero iff @c@ represents the java.lang.Object class.
-}
data Class_ cls str super = Mk_class {
        _minor :: Word16 -- ^ class file minor version
        , _major :: Word16 -- ^ class file major version
        , _pool :: Pool_ cls str -- ^ constant pool
        , _access :: Access -- ^ public, protected, etc.
        , _this :: cls -- ^ (Class) of this class
        , _super :: super -- ^ (Class) of the superclass (parent class) of this class
        , _ifaces :: [cls] -- ^ interfaces implemented by this class
        , _fields :: [Field_info str] -- ^ fields of this class
        , _methods :: [Method_info str] -- ^ methods of this class
        , _attrs :: [Attribute str] -- ^ usually ignored
    } deriving (Read, Show)

class C_get_name a b where
    -- | The parameter @a@ can be a @'Class' 'Raw'@ or a @'Class' 'Resolved'@.
    get_name :: a -> b

instance (cls0 ~ cls1) => C_get_name (Class_ cls0 str super) cls1 where
    get_name = _this

-- type PoolC a = [a] -- slow
type PoolC a = Vector a

type Pool_ cls str = PoolC (Entry_ cls str)

type Pool r = PoolC (Entry r)

-- | @get_constant_pool :: 'Class' r -> 'Pool' r@
get_constant_pool :: Class_ cls str super -> Pool_ cls str
get_constant_pool = _pool

data Attribute str = Mk_attribute {
        a_name :: str
        , a_content :: ByteString
    } deriving (Read, Show)

data Field_info str = Mk_field_info {
        fi_access :: Access
        , fi_name :: str
        , fi_descriptor :: Index -- ^ not implemented
        , fi_attributes :: [Attribute str]
    } deriving (Read, Show)

data Method_info str = Mk_method_info {
        mi_access :: Access
        , mi_name :: str
        , mi_descriptor :: Index -- ^ not implemented
        , mi_attributes :: [Attribute str]
    } deriving (Read, Show)

-- | Access modifier (public, private, etc.)
type Access = Word16

-- | UTF-8-encoded 'ByteString'.
type Ubs = ByteString

-- | One-based index.
type Index = Word16

-- | An entry of a constant pool.
data Entry_ cls str
    = Utf8 Ubs
    | Integer Int32
    | Float Float
    | Long Int64
    | Double Double
    | EClass cls
    | String str
    | Fieldref Index Index -- ^ not implemented
    | Methodref Index Index -- ^ not implemented
    | Interfacemethodref Index Index -- ^ not implemented
    | Nameandtype str cls -- ^ part of field or method info
    | Unused -- ^ second slot of 8-byte constant
    deriving (Read, Show)

-- | Replace indexes with contents.
resolve_constant_pool :: (Monad m) => Pool Raw -> m (Pool Resolved)
resolve_constant_pool raws = mapM (resolve_entry raws) raws

-- | Internal function.
resolve_entry :: (Monad m) => Pool Raw -> Entry Raw -> m (Entry Resolved)
resolve_entry raws raw = case raw of
    Utf8 x -> return $ Utf8 x
    Integer x -> return $ Integer x
    Float x -> return $ Float x
    Long x -> return $ Long x
    Double x -> return $ Double x
    EClass c -> do
        Utf8 s <- raws `at` (c - 1)
        return $ EClass s
    String s -> do
        Utf8 s0 <- raws `at` (s - 1)
        return $ String s0
    Fieldref x y -> return $ Fieldref x y
    Methodref x y -> return $ Methodref x y
    Interfacemethodref x y -> return $ Interfacemethodref x y
    Nameandtype s c -> do
        Utf8 s0 <- raws `at` (s - 1)
        Utf8 c0 <- raws `at` (c - 1)
        return $ Nameandtype s0 c0
    Unused -> return Unused
