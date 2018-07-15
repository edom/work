-- | This module should be renamed to @Meta.JvmPrelude@.
module Meta.JvmAccess (
    -- * Type synonyms
    Access
    , PIndex
    , Pc
    -- * Common properties
    , Get_access(..)
    , Get_code(..)
    , Get_name(..)
    , Get_content(..)
    , Get_attributes(..)
    -- * Reexports
    , module Meta.Prelude
) where

import Prelude ()
import Meta.Prelude

-- | Access modifier (public, protected, package-private, private).
type Access = Word16

{- |
One-based index into constant pool.
The first constant pool entry has index 1.
-}
type PIndex = Word16

-- | Program counter.
type Pc = Word16

-- | Usually @b@ is 'Access'.
class Get_access a b where
    get_access :: a -> b

class Get_code a b where
    get_code :: a -> b

class Get_name a b where
    get_name :: a -> b

-- | Usually @b@ is 'ByteString'.
class Get_content a b where
    get_content :: a -> b

class Get_attributes a b where
    get_attributes :: a -> b
