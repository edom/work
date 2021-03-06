module Meta.MavenDep where

-- * Dependency

type Group_id = String

type Artifact_id = String

-- | Dependency version specification syntax is documented in 'version'.
type Dep_ver = String

data Scope
    = Compile
    | Provided
    deriving (Eq, Ord, Read, Show)

data Dep
    -- | Internal. Do not use. Use dependency constructors.
    = MkDep {
        groupId :: Group_id
        , artifactId :: Artifact_id
        -- | syntax is documented in <https://maven.apache.org/pom.html#Dependency_Version_Requirement_Specification>
        , version :: Dep_ver
        , scope :: Scope
    } deriving (Eq, Ord, Read, Show)

-- * Dependency constructors

-- | Compile-time dependency.
compile
    :: Maven_group_id -- ^ group id
    -> Maven_artifact_id -- ^ artifact id
    -> Dep_ver -- ^ see 'Dep_ver' for syntax
    -> Dep

compile grp art ver = MkDep grp art ver Compile

provided :: Group_id -> Artifact_id -> Dep_ver -> Dep
provided grp art ver = MkDep grp art ver Provided

-- * Renamings in order to reduce name clashes

type Maven_group_id = Group_id

type Maven_artifact_id = Artifact_id

type Maven_dep = Dep

type Maven_dep_ver = Dep_ver
