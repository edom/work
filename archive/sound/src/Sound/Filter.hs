module Sound.Filter
(
    lzero2cp
    , lpole2cp
    , rlzero2cp
    , rlpole2cp
    , Cx
    , Z
)
where

import qualified Data.Complex as Cp

import Sound.InfList
import Sound.Time

lpole2cp :: Z -> L Double -> L Double
lpole2cp p x0 =
    y0
    where
        y0 = lzip3 (\ sy1 sy2 sx0 -> a * sy1 - b * sy2 + sx0) y1 y2 x0
        y1 = ldelay y0
        y2 = ldelay y1
        r = Cp.realPart p
        i = Cp.imagPart p
        a = 2 * r
        b = r * r + i * i

rlpole2cp :: Z -> Rated (L Double) -> Rated (L Double)
rlpole2cp p = rmap (lpole2cp p)

lzero2cp :: Z -> L Double -> L Double
lzero2cp p x0 =
    lzip3 (\ sx0 sx1 sx2 -> sx0 - a * sx1 + b * sx2) x0 x1 x2
    where
        x1 = ldelay x0
        x2 = ldelay x1
        r = Cp.realPart p
        i = Cp.imagPart p
        a = 2 * r
        b = r * r + i * i

rlzero2cp :: Z -> Rated (L Double) -> Rated (L Double)
rlzero2cp p = rmap (lzero2cp p)

-- | complex number
type Cx = Cp.Complex Double

-- | z-plane coordinate
type Z = Cx
