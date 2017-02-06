module Haskell.Interpret.Name
(
    Name(..)
    , pretty
)
where

-- | A qualified name.
data Name
    = MkName
    {
        module_ :: String
        , name :: String
    }
    deriving (Read, Show, Eq)

pretty :: Name -> String
pretty (MkName m n) = m ++ "." ++ n
