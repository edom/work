{-# LANGUAGE LambdaCase #-}

module File
where

import qualified Control.Monad as M
import qualified System.IO as SI

import qualified System.Directory as Dir

import qualified System.FilePath as Fp

import qualified Data.Time as Ti

-- * Convenience functions

{- |
This is 'SI.writeFile' from "System.IO"
but this also creates the directories for the file
instead of throwing an exception.
-}
write_file :: FilePath -> String -> IO ()
write_file path content =
    createDirectoryFor path
    >> SI.writeFile path content

-- * Directory creation

{- |
This also creates the parents.

See 'Dir.createDirectoryIfMissing' in "System.Directory".
-}
createDirectory :: FilePath -> IO ()
createDirectory = Dir.createDirectoryIfMissing True

{- |
This creates the directory that would
contain the file whose path is given.
-}
createDirectoryFor :: FilePath -> IO ()
createDirectoryFor = Dir.createDirectoryIfMissing True . Fp.takeDirectory

-- | 'Dir.getModificationTime' in "System.Directory".
getModificationTime :: FilePath -> IO Ti.UTCTime
getModificationTime = Dir.getModificationTime

-- | 'Dir.doesFileExist' in "System.Directory".
doesFileExist :: FilePath -> IO Bool
doesFileExist = Dir.doesFileExist

-- * Path

-- | 'Fp.</>' in "System.FilePath".
(</>) :: FilePath -> FilePath -> FilePath
(</>) = (Fp.</>)
infixr 5 </>

-- * Extension

-- | 'Fp.replaceExtension' in "System.FilePath".
replaceExtension :: FilePath -> String -> FilePath
replaceExtension = Fp.replaceExtension

-- | 'Fp.dropExtension' in "System.FilePath".
dropExtension :: FilePath -> FilePath
dropExtension = Fp.dropExtension

extensionIs :: FilePath -> String -> Bool
extensionIs path ext = Fp.takeExtension path == ext

-- * Find

{- |
Find all files recursively under the given directory.

This follows symbolic links.

Similar to the @find@ utility: @find \<root\> -type f@.

If the path does not point to a directory, this returns an empty list.
-}
files_under :: FilePath -> IO [FilePath]
files_under root = go ""
    where
        go prefix = do
            let parent = root Fp.</> prefix
            Dir.doesDirectoryExist parent >>= \case
                False -> return []
                True -> do
                    contents <- Dir.getDirectoryContents parent
                    fmap concat . M.forM contents $ \ name -> do
                        let
                            relPath = prefix Fp.</> name
                            absPath = parent Fp.</> relPath
                        case name of
                            "." -> return []
                            ".." -> return []
                            _ ->
                                -- doesDirectoryExist x also returns true if x is a symlink to a directory.
                                ifM
                                    (Dir.doesDirectoryExist absPath)
                                    (fmap (relPath :) $ go relPath)
                                    (return [relPath])
        ifM mc mt mf = do
            c <- mc
            if c then mt else mf

-- * Copying files

copy_file
    :: FilePath -- ^ source
    -> FilePath -- ^ destination
    -> IO ()

copy_file src dst = do
    createDirectoryFor dst
    Dir.copyFile src dst
