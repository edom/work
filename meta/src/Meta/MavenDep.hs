module Meta.MavenDep where

-- * Dependency

type GrpId = String
type ArtId = String
type Version = String

-- | Dependency version specification syntax is documented in 'version'.
type DepVer = String

data Scope
    = Compile
    | Provided
    deriving (Read, Show)

data Dep
    -- | Internal. Do not use. Use dependency constructors.
    = MkDep {
        groupId :: GrpId
        , artifactId :: ArtId
        -- | syntax is documented in https://maven.apache.org/pom.html#Dependency_Version_Requirement_Specification
        , version :: Version
        , scope :: Scope
    } deriving (Read, Show)

-- * Dependency constructors

-- | Compile-time dependency.
compile
    :: GrpId -- ^ group id
    -> ArtId -- ^ artifact id
    -> DepVer -- ^ see 'DepVer' for syntax
    -> Dep

compile grp art ver = MkDep grp art ver Compile

provided :: GrpId -> ArtId -> DepVer -> Dep
provided grp art ver = MkDep grp art ver Provided

-- * Predefined dependencies

servletApi :: DepVer -> Dep
servletApi ver = provided "javax.servlet" "javax.servlet-api" ver
