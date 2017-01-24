module Parse.Location
(
    -- * Location

    Location(..)
    , Path

    -- * Located

    , Located(..)
    , locate
)
where

-- | An inhabitant of this type describes where a character is in a text file.
data Location
    = MkLocation
    {
        -- | Path to the file.
        path :: Path
        ,
        -- | Line number starting from one.
        line :: Int
        ,
        -- | Column number starting from one.
        column :: Int
    }
    deriving (Read, Show)

-- | A path in a file system.
type Path = String

-- | This product type adds 'Location' information to another type.
data Located a
    = MkLocated Location a
    deriving (Read, Show)

instance Functor Located where
    fmap f (MkLocated a b) = MkLocated a (f b)

-- | Get the associated 'Location'.
locate :: Located a -> Location
locate (MkLocated x _) = x
