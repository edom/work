module Parse.Location
(
    -- * Location

    Location(..)
    , Path

    -- * Located

    , HasLocation(..)
    , getLocation
    , getColumn
    , Located(..)
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

class HasLocation a where
    -- | Get the associated 'Location'.
    locate :: a -> Location

-- | This product type adds 'Location' information to another type.
data Located a
    = MkLocated Location a
    deriving (Read, Show)

instance Functor Located where
    fmap f (MkLocated a b) = MkLocated a (f b)

instance HasLocation (Located a) where
    locate (MkLocated x _) = x

getLocation :: (HasLocation a) => a -> Location
getLocation = locate

getColumn :: (HasLocation a) => a -> Int
getColumn = column . locate
