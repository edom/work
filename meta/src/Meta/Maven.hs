{-# LANGUAGE ApplicativeDo #-}

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
        doc = X.MkDoc [
                X.proc "xml" [X.atr "version" "1.0", X.atr "encoding" "UTF-8"]
                , X.elm "project" [
                    X.atr "xmlns" "http://maven.apache.org/POM/4.0.0"
                    , X.nAtr "xmlns" "xsi" "http://www.w3.org/2001/XMLSchema-instance"
                    , X.nAtr "xsi" "schemaLocation" "http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"
                ] [
                    X.elm "modelVersion" [] [X.text "4.0.0"]
                    , X.elm "groupId" [] [X.text grp]
                    , X.elm "artifactId" [] [X.text art]
                    , X.elm "version" [] [X.text ver]
                    , X.elm "packaging" [] [X.text $ renderPackaging pkg]
                    , X.elm "properties" [] [
                        X.elm "project.build.sourceEncoding" [] [X.text "UTF-8"]
                        , X.elm "java.version" [] [X.text "1.8"]
                    ]
                    , X.elm "build" [] [
                        X.elm "plugins" [] [
                            X.elm "plugin" [] [
                                X.elm "groupId" [] [X.text "org.apache.maven.plugins"]
                                , X.elm "artifactId" [] [X.text "maven-compiler-plugin"]
                                , X.elm "version" [] [X.text "3.1"]
                                , X.elm "configuration" [] [
                                    X.elm "source" [] [X.text "${java.version}"]
                                    , X.elm "target" [] [X.text "${java.version}"]
                                ]
                            ]
                        ]
                    ]
                    , X.elm "dependencies" [] (map renderDep deps)
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
        renderDep d = X.elm "dependency" [] [
                X.elm "groupId" [] [X.text $ MD.groupId d]
                , X.elm "artifactId" [] [X.text $ MD.artifactId d]
                , X.elm "version" [] [X.text $ MD.version d]
                , X.elm "scope" [] [X.text $ renderScope $ MD.scope d]
            ]
        renderScope scope = case scope of
            MD.Compile -> "compile"
            MD.Provided -> "provided"

data Packaging
    = PJar
    | PPom
    deriving (Read, Show)
