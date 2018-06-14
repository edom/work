{-# LANGUAGE ApplicativeDo #-}

{- |

An example web application.

-}
module Meta.Example where

import qualified Control.Monad as M

import qualified Meta.File as F
import qualified Meta.IntCbp as C
import qualified Meta.Java as J
import qualified Meta.JavaRender as JR
import qualified Meta.Maven as N
import qualified Meta.MavenDep as MD
import qualified Meta.Prop as P
import qualified Meta.Web as W
import qualified Meta.Xml as X

import qualified Meta.ExampleTables as T

main :: IO ()
main = do
    let
        project = (N.mkProject "com.spacetimecat" "example" "0.0.0") {
                N.pDeps = [
                    MD.servletApi "3.1.0"
                ]
            }
        site = W.MkSite [
                W.MkPage "/" (W.CSeq (W.CText "hello world") (W.CLink "/1" (W.CText "page 1")))
                , W.MkPage "/1" (W.CText "this is page 1")
            ]
        intDtos = map (renameDto . C.genDto C.defGenDto) T.tables
    javaDtoClasses <- P.ioFromErr $ sequenceA $ map C.toJavaClass intDtos
    let
        javaServletClass = W.toJavaHttpServletClass "MySiteHttpServlet" site
        javaClasses = map placeJavaClass $ javaDtoClasses ++ [javaServletClass]
    files <- P.ioFromErr $ do
        xml_files <- pure [F.text "pom.xml" $ X.renderDoc X.defRenOpt $ N.toPomXml project]
        java_files <- pure $ map (prependPath "src/main/java/" . JR.renderClassFile JR.defRenOpt) javaClasses
        pure $ xml_files ++ java_files
    M.forM_ (map move files) $ \ file -> do
        putStrLn $ "Writing " ++ F.fPath file
        F.write file
    where
        move file = file { F.fPath = "dist/example/" ++ F.fPath file }
        renameDto cls = cls {
            C.cName = "Row_" ++ C.cName cls
        }
        placeJavaClass cls = cls {
            J.cPkg = "com.spacetimecat"
        }
        prependPath prefix file = file { F.fPath = prefix ++ F.fPath file }
