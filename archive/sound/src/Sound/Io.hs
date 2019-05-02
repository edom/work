{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}

module Sound.Io
(
    ByteCount
    , getFileSize
    -- * General strict byte-string input
    , slurp
    -- * Handle
    , bshPut
    , writingFile
    -- * Types
    , ChunkSampleCount
    -- * Error
    , ioUserError
    -- * Reexports
    , module Control.Applicative
    , module Control.Monad
    , module Data.Int
    , module Data.Word
    , Handle
    , hGetBuf
    , hPutBuf
    , IOMode(..)
    , SeekMode(..)
    , withBinaryFile
    , hSeek
    , IOError
    , ioError
    , userError
)
where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Word
import System.IO
    (
        Handle
        , IOMode(..)
        , SeekMode(..)
        , hGetBuf
        , hPutBuf
        , hSeek
        , withBinaryFile
    )

import qualified Control.Monad as M
import qualified Data.ByteString as Bs

import qualified Filesystem as Fi
import qualified Filesystem.Path.CurrentOS as Fpc

type ByteCount = Int

ioUserError :: String -> IO a
ioUserError = ioError . userError

writingFile :: FilePath -> (Handle -> IO a) -> IO a
writingFile path = withBinaryFile path WriteMode
{-# INLINE writingFile #-}

getFileSize :: FilePath -> IO ByteCount
getFileSize path = do
    let
        maxSize = fromIntegral (maxBound :: Int)
        path' = Fpc.decodeString path
    size <- Fi.getSize path'
    M.unless (size >= 0) $ ioUserError $ path ++ " has negative size: " ++ show size
    -- Limit to one less than maxSize just in case we loop with ascending counter.
    M.unless (size < maxSize) $ ioUserError $ path ++ " is too large: " ++ show size
    return (fromInteger size)

-- | Read the entire file into memory.
slurp :: FilePath -> IO Bs.ByteString
slurp = Bs.readFile

{- |
'Bs.hPut' from "Data.ByteString".
-}
bshPut :: Handle -> Bs.ByteString -> IO ()
bshPut = Bs.hPut

type ChunkSampleCount = Int
