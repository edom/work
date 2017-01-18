{-# OPTIONS -fno-warn-name-shadowing #-}

{- |
The game uses Gregorian calendar as used in 2017
although it did not exist in 1066;
it was introduced in 1582.
-}
module Dynasty.Date
(
    Date

    , fromYmd
    , toYmd
    , parse

    , year

    , increment
    , (-)

    , print
)
where

import Prelude hiding
    (
        Num(..)
        , print
    )

import qualified Data.Time as T

newtype Date = In { out :: T.Day } deriving (Eq, Ord)

fromYmd
    :: Int -- ^ year
    -> Int -- ^ month (1-12)
    -> Int -- ^ day (1-31)
    -> Date

fromYmd year month day = In $ T.fromGregorian (fromIntegral year) month day

toYmd :: Date -> (Int, Int, Int)
toYmd date = (fromIntegral y, m, d)
    where
        (y, m, d) = T.toGregorian $ out date

year :: Date -> Int
year date = fromIntegral y
    where
        (y, _, _) = T.toGregorian $ out date

parse :: (Monad m) => String -> m Date
parse = fmap In . T.parseTimeM False T.defaultTimeLocale "%F"

print :: Date -> String
print = T.formatTime T.defaultTimeLocale "%F" . out

(-) :: Date -> Date -> Int
(-) a b = fromIntegral $ T.diffDays (out a) (out b)
infixl 6 -

increment :: Date -> Date
increment = In . T.addDays 1 . out
