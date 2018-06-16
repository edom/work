module Meta.File where

import qualified System.Directory as D
import qualified System.FilePath as P
import qualified System.IO as I

data File
    -- | Internal. Do not use. Use 'text'.
    = MkFile {
        fPath :: I.FilePath
        , fContent :: String
    } deriving (Read, Show)

-- * Constructors

text :: I.FilePath -> String -> File
text = MkFile

-- * Actions

-- | deprecated
prependPath :: I.FilePath -> File -> File
prependPath pre fil = fil { fPath = pre ++ fPath fil }

prepend_path :: I.FilePath -> File -> File
prepend_path pre fil = fil { fPath = pre ++ fPath fil }

write :: File -> IO ()
write file = do
    D.createDirectoryIfMissing recursive dir
    I.writeFile path content
    where
        recursive = True
        dir = P.takeDirectory path
        path = fPath file
        content = fContent file
