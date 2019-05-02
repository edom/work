{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Route where

import qualified Control.Monad.Trans.State as S

import qualified Text.Pandoc as P

import qualified Dictionary as Di
import qualified File as F
import qualified Metadata as Md

class (Monad m) => MonadRoute m where
    {- |
Prepend a subpath to the output path;
moves the output into a directory whose path is given.
    -}
    moveInto :: FilePath -> m ()
    replaceExtension :: String -> m ()

instance Md.MonadMetadata (S.State Env) where
    hasMetadata k = Di.member k <$> S.gets _re_pandoc

instance MonadRoute (S.State Env) where
    moveInto p = S.modify $ \ s -> s { _re_path = p F.</> _re_path s }
    replaceExtension e = S.modify $ \ s -> s { _re_path = F.replaceExtension (_re_path s) e }

instance MonadRoute (S.State FilePath) where
    moveInto p = S.modify (p F.</>)
    replaceExtension e = S.modify (`F.replaceExtension` e)

data Env
    = MkEnv
    {
        _re_path :: FilePath
        , _re_pandoc :: P.Pandoc
    }
    deriving (Show, Read)
