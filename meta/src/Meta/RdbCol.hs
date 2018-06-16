module Meta.RdbCol where

-- | Database column name.
type DbColName = String

-- | Database column data type.
data Type
    = TInt32
    | TInt64
    | TVarChar Int -- ^ the limit is the maximum number of characters, not bytes
    | TNullable Type -- ^ TODO move to Col cNullable ?
    deriving (Read, Show)

-- * Column

-- | View table column title.
type ShortTitle = String

-- | Form field title.
type LongTitle = String

-- | Form field description.
type FormDesc = String

{- |
A 'Col' represents a database table column.
-}
data Col
    -- | Internal. Do not use. Use column constructors, getters, and setters.
    = MkCol {
        cType :: Type
        , cName :: DbColName
        , cShortTitle :: Maybe ShortTitle
        , cLongTitle :: Maybe LongTitle
        , cFormDesc :: Maybe FormDesc
    } deriving (Read, Show)

defCol :: Col
defCol = MkCol {
        cType = TInt32
        , cName = ""
        , cShortTitle = Nothing
        , cLongTitle = Nothing
        , cFormDesc = Nothing
    }

-- * Constructors

mkCol :: Type -> DbColName -> Col
mkCol t n = defCol { cType = t, cName = n }

colInt32 :: DbColName -> Col
colInt32 name = defCol {
        cType = TInt32
        , cName = name
    }

colInt64 :: DbColName -> Col
colInt64 name = defCol {
        cType = TInt64
        , cName = name
    }

colVarChar :: Int -> DbColName -> Col
colVarChar limit name = defCol {
        cType = TVarChar limit
        , cName = name
    }

-- * Getters

getType :: Col -> Type
getType = cType

getName :: Col -> String
getName = cName

getShortTitle :: Col -> Maybe ShortTitle
getShortTitle = cShortTitle

getLongTitle :: Col -> Maybe LongTitle
getLongTitle = cShortTitle

-- * Setters

setShortTitle :: ShortTitle -> Col -> Col
setShortTitle t c = c { cShortTitle = Just t }

setLongTitle :: LongTitle -> Col -> Col
setLongTitle t c = c { cLongTitle = Just t }
