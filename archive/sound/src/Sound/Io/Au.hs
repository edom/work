module Sound.Io.Au
(
    header
    , Rate
    , Count
)
where

import Data.Word

import qualified Data.ByteString as Bs

import qualified Data.Serialize as Se

type Rate = Word32
type Count = Word32

{- |
Make a 24-byte AU format header for 64-bit float sample, 1 channel,
and the given sample rate and sample count.
-}
header :: Rate -> Count -> Bs.ByteString
header rate numSamples =
    Se.runPut $ mapM_ Se.putWord32be [magic, dataOffset, dataSize, encoding, rate, nchan]
    where
        magic = 0x2e736e64 -- .snd
        dataOffset = 24
        dataSize = 8 * numSamples
        encoding = 7 -- double
        nchan = 1
