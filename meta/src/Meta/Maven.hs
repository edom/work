module Meta.Maven where

import qualified Meta.File as F

-- * Dependency

data Dep
    -- | Internal. Do not use. Use dependency constructors.
    = MkDep {
        dGroupId :: String
        , dArtifactId :: String
        -- | syntax is documented in https://maven.apache.org/pom.html#Dependency_Version_Requirement_Specification
        , dVersion :: String
    } deriving (Read, Show)

-- * Dependency constructors

-- | Compile-time dependency.
depCompile
    :: String -- ^ group id
    -> String -- ^ artifact id
    -> String -- ^ version; syntax is documented in 'dVersion'
    -> Dep

depCompile grp art ver = MkDep grp art ver

-- * Project

data Project
    -- | Internal. Do not use. Use 'mkProject'.
    = MkProject {
        pGroupId :: String
        , pArtifactId :: String
        , pVersion :: String
        , pParent :: Maybe Project
        , pPackaging :: Packaging
        , pDeps :: [Dep]
    } deriving (Read, Show)

-- | Internal. Do not use.
data Packaging
    = PJar
    | PPom
    deriving (Read, Show)

mkProject
    :: String -- ^ group id
    -> String -- ^ artifact id
    -> String -- ^ version
    -> Project

mkProject grp art ver = MkProject {
        pGroupId = grp
        , pArtifactId = art
        , pVersion = ver
        , pParent = Nothing
        , pPackaging = PJar
        , pDeps = []
    }

{- |
https://maven.apache.org/guides/introduction/introduction-to-the-pom.html#Minimal_POM

https://gist.github.com/torgeir/6742158

Search the Internet for @pom.xml template@.
-}
genPomXml :: Project -> F.File
genPomXml pro = F.text "pom.xml" content
    where
        content =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            ++ "<project xmlns=\"http://maven.apache.org/POM/4.0.0\"\n"
            ++ "        xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
            ++ "        xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">\n"
            ++ "    <modelVersion>4.0.0</modelVersion>\n"
            ++ "    <groupId>" ++ grp ++ "</groupId>\n"
            ++ "    <artifactId>" ++ art ++ "</artifactId>\n"
            ++ "    <version>" ++ ver ++ "</version>\n"
            ++ "    <packaging>" ++ renderPackaging pkg ++ "</version>\n"
            ++ "    <properties>\n"
            ++ "        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>\n"
            ++ "        <java.version>1.8</java.version>\n"
            ++ "    </properties>\n"
            ++ "    <build>\n"
            ++ "        <plugins>\n"
            ++ "            <plugin>\n"
            ++ "                <groupId>org.apache.maven.plugins</groupId>\n"
            ++ "                <artifactId>maven-compiler-plugin</artifactId>\n"
            ++ "                <version>3.1</version>\n"
            ++ "                <configuration>\n"
            ++ "                    <source>${java.version}</source>\n"
            ++ "                    <target>${java.version}</target>\n"
            ++ "                </configuration>\n"
            ++ "            </plugin>\n"
            ++ "        </plugins>\n"
            ++ "    </build>\n"
            ++ "</project>\n"
        grp = pGroupId pro
        art = pArtifactId pro
        ver = pVersion pro
        pkg = pPackaging pro
        renderPackaging p = case p of
            PJar -> "jar"
            PPom -> "pom"
