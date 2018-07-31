{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Orphan instances.
module Trade_orphan where

import qualified Control.Monad.Reader as R

import qualified Meta.Crypto as Crypto

instance (Crypto.MonadRandom m) => Crypto.MonadRandom (R.ReaderT r m) where
    getRandomBytes n = R.lift $ Crypto.getRandomBytes n
