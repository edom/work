{- |
A list represents a discrete signal.

Works but slow.
-}
module Sound.List
(
    silence
)
where

silence :: (Num a) => [a]
silence = repeat 0
