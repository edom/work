{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
This module wraps the @zip-archive@ package.
-}
module Meta.Zip (
    Archive
    , get_entries
    , Entry
    , decompress_or_error
    , get_relative_path
    , Slurp(..)
) where

import qualified Control.Monad.IO.Class as MI
import qualified Meta.ByteString as B
import qualified Meta.Prelude as P
import qualified Codec.Archive.Zip as Z

-- | See 'Z.Archive'.
type Archive = Z.Archive

get_entries :: Archive -> [Entry]
get_entries = Z.zEntries

type Entry = Z.Entry

-- | This is a partial function.
decompress_or_error :: Entry -> B.ByteString
decompress_or_error = B.toStrict . Z.fromEntry

get_relative_path :: Entry -> FilePath
get_relative_path = Z.eRelativePath

{- |
See note in 'Z.toArchiveOrFail' about @binary >= 0.7@.
-}
class Slurp m a where
    slurp :: FilePath -> m a

instance (MI.MonadIO m) => Slurp m Archive where
    slurp = P.slurp P.>=> P.raise_either . Z.toArchiveOrFail

instance (MI.MonadIO m) => Slurp m Entry where
    slurp = MI.liftIO . Z.readEntry []
