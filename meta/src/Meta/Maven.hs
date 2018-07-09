{-# LANGUAGE RecordWildCards #-}

{- |

* Use 'mk_project' to describe your project.

* Use 'to_pom_xml' to transform that description to XML document.

-}
module Meta.Maven (
    -- * Coordinates
    MD.Group_id
    , MD.Artifact_id
    , Version
    -- * Project
    , Project(..)
    , empty
    , mk_project
    , set_gav
    -- * Dependency
    , MD.Dep
    , MD.Dep_ver
    , set_deps
    , dep_provided
    -- * POM XML
    , to_pom_xml
    , Packaging
    -- * Renamings
    , MD.Maven_group_id
    , MD.Maven_artifact_id
    , Maven_version
    , MD.Maven_dep
    , MD.Maven_dep_ver
) where

import qualified Meta.MavenDep as MD
import qualified Meta.Xml as X
import qualified Meta.Xml_0 as Y

type Version = String

type Maven_version = Version

data Project
    -- | Internal. Do not use. Use 'mk_project'.
    = MkProject {
        pGroupId :: MD.Group_id
        , pArtifactId :: MD.Artifact_id
        , pVersion :: Version
        , pParent :: Maybe Project
        , pPackaging :: Packaging
        , pDeps :: [MD.Dep]
    } deriving (Read, Show)

empty :: Project
empty = MkProject {
        pGroupId = ""
        , pArtifactId = ""
        , pVersion = ""
        , pParent = Nothing
        , pPackaging = PJar
        , pDeps = []
    }

mk_project :: MD.Group_id -> MD.Artifact_id -> Version -> Project
mk_project grp art ver = set_gav grp art ver empty

set_gav :: MD.Group_id -> MD.Artifact_id -> Version -> Project -> Project
set_gav g a v p = p {
        pGroupId = g
        , pArtifactId = a
        , pVersion = v
    }

set_deps :: [MD.Dep] -> Project -> Project
set_deps ds p = p { pDeps = ds }

dep_provided :: MD.Group_id -> MD.Artifact_id -> MD.Dep_ver -> MD.Dep
dep_provided = MD.provided

{- |
https://maven.apache.org/guides/introduction/introduction-to-the-pom.html#Minimal_POM

https://gist.github.com/torgeir/6742158

Search the Internet for @pom.xml template@.
-}
to_pom_xml :: Project -> X.Doc
to_pom_xml pro = doc
    where
        doc :: X.Doc
        doc = X.mkDoc [
                X.proc "xml" [atr "version" "1.0", atr "encoding" "UTF-8"]
                , elm "project" [
                    atr "xmlns" "http://maven.apache.org/POM/4.0.0"
                    , nAtr "xmlns" "xsi" "http://www.w3.org/2001/XMLSchema-instance"
                    , nAtr "xsi" "schemaLocation" "http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"
                ] [
                    elm "modelVersion" [] [text "4.0.0"]
                    , elm "groupId" [] [text grp]
                    , elm "artifactId" [] [text art]
                    , elm "version" [] [text ver]
                    , elm "packaging" [] [text $ renderPackaging pkg]
                    , elm "properties" [] [
                        elm "project.build.sourceEncoding" [] [text "UTF-8"]
                        , elm "java.version" [] [text "1.8"]
                    ]
                    , elm "build" [] [
                        elm "plugins" [] [
                            elm "plugin" [] [
                                elm "groupId" [] [text "org.apache.maven.plugins"]
                                , elm "artifactId" [] [text "maven-compiler-plugin"]
                                , elm "version" [] [text "3.1"]
                                , elm "configuration" [] [
                                    elm "source" [] [text "${java.version}"]
                                    , elm "target" [] [text "${java.version}"]
                                ]
                            ]
                        ]
                    ]
                    , elm "dependencies" [] (map renderDep deps)
                ]
            ]
        grp = pGroupId pro
        art = pArtifactId pro
        ver = pVersion pro
        pkg = pPackaging pro
        deps = pDeps pro
        renderPackaging p = case p of
            PJar -> "jar"
            PPom -> "pom"
        renderDep d = elm "dependency" [] [
                elm "groupId" [] [text $ MD.groupId d]
                , elm "artifactId" [] [text $ MD.artifactId d]
                , elm "version" [] [text $ MD.version d]
                , elm "scope" [] [text $ renderScope $ MD.scope d]
            ]
        renderScope scope = case scope of
            MD.Compile -> "compile"
            MD.Provided -> "provided"
        Y.Module_Meta_Xml{..} = Y.module_Meta_Xml

data Packaging
    = PJar
    | PPom
    deriving (Read, Show)
