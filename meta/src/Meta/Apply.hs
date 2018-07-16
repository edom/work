{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Experimental.
This tries to overload function application, although Haskell doesn't really allow that.

To-do:

    * <https://wiki.haskell.org/GHC/AdvancedOverlap>

    * Add the following phase to GHC, after parsing but before typechecking:

        * Rewrite every @HsApp a b@ into @HsApp (HsApp \<apply\> a) b@.

            * @\<apply\>@ is a @HsVar@ that refers to 'apply'.
-}
module Meta.Apply where

class App' flag a b c where app' :: flag -> a -> b -> c
instance App' Fal (a -> b) a b where app' _ = ($)
instance App' Tru (a -> b) a b where app' _ = ($)
instance (App' Fal f a b, Monad m, m ~ n) => App' Tru f (m a) (n b) where app' _ f ma = (\ a -> app' (undefined :: Fal) f a) <$> ma
instance (App' Tru f a b, Monad m, m ~ n) => App' Fal f (m a) (n b) where app' _ f ma = (\ a -> app' (undefined :: Tru) f a) <$> ma

apply :: (App' Fal f a b) => f -> a -> b
apply = app' (undefined :: Fal)

data Fal
data Tru

-- class App' mon a b c where app' :: mon -> a -> b -> c
-- instance (Monadic m a c b) => App 

x :: (Monad m) => m Int
x = return 0

f :: Int -> Int
f x = x + 1

g :: (Monad m) => m Int -> m Int
g = fmap (1 +)

k :: (Monad m) => Int -> m Int
k x = pure $ x + 1

y :: (Monad m) => m (m Int)
y = return $ return 1

z :: Maybe (Maybe (Maybe Int))
z = return $ return $ return 1

fg :: (Monad m) => m (m Int)
fg = f `apply` y

ff :: Maybe (Maybe (Maybe Int))
ff = f `apply` z

class Count a where
    count :: a -> Int

instance Count Int where
    count _ = 0

instance (Count a) => Count (f a) where
    count fa = 1 + count (un fa)
        where
            un :: f a -> a
            un = undefined

class Inc a where
    inc :: a -> a

instance Inc Int where
    inc = (1 +)

instance (Monad m, Inc a) => Inc (m a) where
    inc = fmap inc

bar :: Maybe (Maybe (Maybe (Maybe Int)))
bar = return $ return $ return $ return 10

de = count bar

fu = inc bar
