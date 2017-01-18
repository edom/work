{-# OPTIONS -fno-warn-name-shadowing #-}

{- |
The game uses Gregorian calendar as used in 2017
although it did not exist in 1066;
it was introduced in 1582.
-}
module Dynasty.Date
(
    -- * Type

    Date
    , Year
    , Month
    , Day

    -- * Wrap

    , fromYmd

    -- * Unwrap

    , toYmd
    , year

    -- * Arithmetics

    , increment
    , (-)

    -- * Parse

    , parse

    -- * Unparse

    , print
)
where

import Prelude hiding
    (
        Num(..)
        , print
    )

import qualified Data.Time as T

{- |
A day in the Gregorian calendar.
-}
newtype Date = In { out :: T.Day } deriving (Eq, Ord)

{- |
The game should not go farther than a million years before Christ was born.
-}
type Year = Int

-- | 1-12
type Month = Int

-- | 1-31
type Day = Int

fromYmd
    :: Year -- ^ year
    -> Month -- ^ month (1-12)
    -> Day -- ^ day (1-31)
    -> Date

fromYmd year month day = In $ T.fromGregorian (fromIntegral year) month day

toYmd :: Date -> (Year, Month, Day)
toYmd date = (fromIntegral y, m, d)
    where
        (y, m, d) = T.toGregorian $ out date

year :: Date -> Year
year date = fromIntegral y
    where
        (y, _, _) = T.toGregorian $ out date

parse
    :: (Monad m)
    => String -- ^ something like @"2017-01-18"@
    -> m Date

parse = fmap In . T.parseTimeM False T.defaultTimeLocale "%F"

print :: Date -> String
print = T.formatTime T.defaultTimeLocale "%F" . out

{- |
Compute the number of days that has to be added to the first date
in order to produce the second date.

If the dates are equal, the result is zero.

This satisfies the identity:

@
'increment' x - x == 1
@
-}
(-) :: Date -> Date -> Int
(-) a b = fromIntegral $ T.diffDays (out a) (out b)
infixl 6 -

{- |
Compute the date one day after the given date.
-}
increment :: Date -> Date
increment = In . T.addDays 1 . out
