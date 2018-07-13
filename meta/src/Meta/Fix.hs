module Meta.Fix (
    Fix(..)
    , Inject(..)
) where

-- | Fix :: (* -> *) -> *
newtype Fix f = In { out :: f (Fix f) }

class Inject a b where
    inject :: a -> b

instance Inject (f (Fix f)) (Fix f) where
    inject =  In

instance Inject (Fix f) (f (Fix f)) where
    inject =  out
