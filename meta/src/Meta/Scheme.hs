{-
* See also:

    * http://hackage.haskell.org/package/husk-scheme
-}
module Meta.Scheme where

data Val
    = Int Int
    | Str String
    deriving (Read, Show)
