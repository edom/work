-- | This module should be qualified-imported.
module Sound.Sox
(
    -- * Building command-line arguments
    mkArgs
    -- * Operands
    , operand
    , autodetect
    , Operand
    -- ** Special file paths
    , stdin
    , stdout
    -- * Formats
    , bits
    , channels
    , rate
    , Format
    -- ** Endianness
    , endian
    , big
    , little
    , Endian
    -- ** File types
    , fileType
    , raw
    , snd
    , wav
    , FileType
    -- ** Encodings
    , encoding
    , signedInteger
    , floatingPoint
    , Encoding
    -- * Effects
    , setChannels
    , normalize
    , Effect
    -- * Type hints
    , Dbfs
    , NumBit
    , NumChan
    , Rate
)
where

import Data.Monoid
import Prelude hiding (snd)

newtype Format
    = MkFormat [String]

instance Monoid Format where
    mempty = MkFormat []
    mappend (MkFormat x) (MkFormat y) = MkFormat (mappend x y)

-- | Number of bits per sample.
bits :: NumBit Int -> Format
bits n = MkFormat ["--bits", show n]

type NumBit a = a

-- | Number of channels.
channels :: NumChan Int -> Format
channels n = MkFormat ["--channels", show n]

type NumChan a = a

-- | Number of samples per second.
rate :: Rate Int -> Format
rate n = MkFormat ["--rate", show n]

type Rate a = a

-- | Sample type.
encoding :: Encoding -> Format
encoding (MkEncoding e) = MkFormat ["--encoding", e]

newtype Encoding
    = MkEncoding String

signedInteger :: Encoding
signedInteger = MkEncoding "signed-integer"

floatingPoint :: Encoding
floatingPoint = MkEncoding "floating-point"

newtype FileType
    = MkFileType String

raw :: FileType
raw = MkFileType "raw"

snd :: FileType
snd = MkFileType "snd"

wav :: FileType
wav = MkFileType "wav"

fileType :: FileType -> Format
fileType (MkFileType x) = MkFormat ["--type", x]

newtype Endian
    = MkEndian String

endian :: Endian -> Format
endian (MkEndian x) = MkFormat ["--endian", x]

big :: Endian
big = MkEndian "big"

little :: Endian
little = MkEndian "little"

data Operand
    = MkOperand Format FilePath

stdin :: FilePath
stdin = "-"

stdout :: FilePath
stdout = "-"

autodetect :: FilePath -> Operand
autodetect = MkOperand mempty

operand :: Format -> FilePath -> Operand
operand = MkOperand

-- | Set the number of channels.
setChannels :: NumChan Int -> Effect
setChannels n = MkEffect ["channel", show n]

normalize :: Dbfs Int -> Effect
normalize dbfs = MkEffect ["gain", "-n", show dbfs]

type Dbfs a = a

data Effect
    = MkEffect [String]

-- | This does not include the executable.
mkArgs :: [Operand] -> Operand -> [Effect] -> [String]
mkArgs inputs output effects =
    ["--buffer", "131072", "--multi-threaded"]
    ++ concatMap unOp inputs
    ++ unOp output
    ++ concatMap unEf effects
    where
        unOp (MkOperand (MkFormat fmt) path) = fmt ++ [path]
        unEf (MkEffect x) = x
