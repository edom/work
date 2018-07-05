{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Manage.Cast where

-- | A predicate that indicates that the type @a@
-- can be simply casted to the type @b@.

class Cast a b where
    cast :: a -> b

instance Cast a a where
    cast = id
