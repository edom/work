{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
This module wraps the @zip-archive@ package.

Make sure that you use @binary >= 0.7@.
See note in 'Z.toArchiveOrFail'.
-}
module Meta.Zip (
    -- * Reading archives that are already in memory
    Archive
    , to_archive
    -- * Obtaining entries
    , get_entries
    , Entry
    , get_relative_path
    -- * Decompressing entries
    , decompress_or_error
    -- * Experimental
    , print_entries_recursive
    , is_zip_file
) where

import qualified Meta.ByteString as B
import qualified Meta.Prelude as P
import qualified Codec.Archive.Zip as Z

{- |
An archive is a list of 'Entry's.

See 'Z.Archive' in the "Codec.Archive.Zip" module in the @zip-archive@ package.
-}
type Archive = Z.Archive

class To_archive a where
    {- |
Read the thing to archive.

The parameter @a@ can be 'P.LazyByteString' or 'P.ByteString'.
    -}
    to_archive :: (Monad m) => a -> m Archive

instance To_archive P.LazyByteString where
    to_archive = either fail return . Z.toArchiveOrFail

instance To_archive P.ByteString where
    to_archive = to_archive . B.fromStrict

-- | This is 'Z.zEntries'.
get_entries :: Archive -> [Entry]
get_entries = Z.zEntries

{- |
This is 'Z.Entry'.

An entry is not yet decompressed.
Use 'decompress_or_error' to decompress it.
-}
type Entry = Z.Entry

-- | This is a partial function.
-- Unfortunately @zip-archive@ doesn't provide a total function.
decompress_or_error :: Entry -> B.ByteString
decompress_or_error = B.toStrict . Z.fromEntry

-- | This is 'Z.eRelativePath'.
get_relative_path :: Entry -> FilePath
get_relative_path = Z.eRelativePath

{- |
Print each entry in the archive on its own line.
The file path is prepended to every line.

If an entry name ends with @.zip@ or @.jar@ (ignoring case),
then the entry is assumed to be an 'Archive',
and this function recurs to that.
-}
print_entries_recursive :: FilePath -> Z.Archive -> IO ()
print_entries_recursive = go
    where
        go prefix archive = do
            P.forM_ (get_entries archive) $ \ entry -> do
                let path = get_relative_path entry
                if is_zip_file path
                    then do
                        putStrLn $ prefix ++ "/" ++ path
                        let dent = decompress_or_error entry
                        arch <- to_archive dent
                        go (prefix ++ "/" ++ path) arch
                    else putStrLn $ prefix ++ "/" ++ path

-- | Whether the file path ends with @.zip@ or @.jar@ (ignoring case).
is_zip_file :: FilePath -> Bool
is_zip_file path =
    (d `P.endsWith` ".zip") || (d `P.endsWith` ".jar")
    where
        d = P.downcase path
