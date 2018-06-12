{- |

* Input/output

    * "Meta.File": generate files

* Relational databases

    * "Meta.Relat": a subset of relational databases, mostly SQL databases

* Intermediate programming languages

    * "Meta.IntCbp": intermediate class-based programming language similar to C, C++, Java, and C#

* Java

    * "Meta.Java": a subset of Java
    * "Meta.Maven": a subset of Maven (dependency management, build system, and central repository, for Java)

-}
module Meta.Readme where

import qualified Control.Monad as M

import qualified Meta.File as F
import qualified Meta.IntCbp as C
import qualified Meta.Maven as N
import qualified Meta.Relat as R

-- * Example

t_order :: R.Table
t_order = R.defTable {
        R.tName = "order"
        , R.tCols = [
            c_id
            , R.colVarChar 16 "ref_num"
            , R.colInt64 "sku_id"
        ]
        , R.tConstraints = [
            R.KPrimaryKey [c_id]
        ]
    }
    where
        c_id = R.colInt64 "id"

t_sku :: R.Table
t_sku = R.defTable {
        R.tName = "sku"
        , R.tCols = [
            c_id
            , R.colVarChar 16 "code"
            , R.colVarChar 128 "name"
        ]
        , R.tConstraints = [
            R.KPrimaryKey [c_id]
        ]
    }
    where
        c_id = R.colInt64 "id"

tables :: [R.Table]
tables = [
        t_sku
        , t_order
    ]

dto_classes :: [C.Class]
dto_classes = map (move . C.genDto C.defGenDto) tables
    where
        move cls = cls {
            C.cPkg = "com.spacetimecat"
            , C.cName = "Row_" ++ C.cName cls
        }

dao_files :: [F.File]
dao_files = map C.renderJavaClass dto_classes

project :: N.Project
project = (N.mkProject "com.spacetimecat" "example" "0.0.0")

files :: [F.File]
files = xml_files ++ java_files
    where
        xml_files = [N.genPomXml project]
        java_files = map (prependPath "src/main/java/") dao_files
        prependPath prefix file = file { F.fPath = prefix ++ F.fPath file }

main :: IO ()
main = do
    M.forM_ (map move files) $ \ file -> do
        putStrLn $ "Generating " ++ F.fPath file
        -- putStrLn $ "//////// " ++ F.fPath file
        -- putStrLn $ F.fContent file
        -- putStrLn ""
        F.write file
    where
        move file = file { F.fPath = "dist/example/" ++ F.fPath file }
