module Meta.Fix (
    Fix(..)
) where

newtype Fix f = In { out :: f (Fix f) }
