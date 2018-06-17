module Meta.File (

    -- * Types

    File
    , Path_rel
    , Content

    -- * Constructors

    , text

    -- * Getters

    , get_path
    , get_content

    -- * Manipulations

    , prepend_dir

    -- * Disk-touching

    , write

) where

import qualified System.Directory as D
import qualified System.FilePath as P
import qualified System.IO as I

data File
    -- | Internal. Do not use. Use 'text'.
    = MkFile {
        fPath :: Path_rel
        , fContent :: Content
    } deriving (Read, Show)

type Path_rel = I.FilePath

type Content = String

get_path :: File -> Path_rel
get_path = fPath

get_content :: File -> Content
get_content = fContent

text :: Path_rel -> Content -> File
text path content =
    if P.isRelative path
        then MkFile path content
        else error $ "Meta.File.text: path must be relative: " ++ show path

-- | This does not touch the disk.
prepend_dir :: I.FilePath -> File -> File
prepend_dir dir fil =
    if P.isRelative new_path
        then fil { fPath = new_path }
        else error $ "Meta.File.prepend_dir: resulting path is not relative: " ++ show new_path
    where
        new_path = dir P.</> fPath fil

-- | Write the file to disk.
write :: File -> IO ()
write file = do
    D.createDirectoryIfMissing recursive dir
    I.writeFile path content
    where
        recursive = True
        dir = P.takeDirectory path
        path = fPath file
        content = fContent file
