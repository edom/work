{- |
This assumes that Docker CLI has been installed in PATH.
-}
module Meta.Docker (
    Deploy
    , Tag
    , Repository
    , mk_deploy_or_error
    , build
    , run
    , push
) where

import Prelude ()
import Meta.Prelude
import qualified Meta.Os as Os

type Tag = String

type Repository = String

data Deploy = MkDeploy {
        _tag :: Tag
        , _repository :: Repository
    } deriving (Read, Show)

mk_deploy_or_error :: Tag -> Repository -> Deploy
mk_deploy_or_error tag repository
    | null tag = error "Meta.Docker.mk_deploy_or_error: tag cannot be empty"
    | null repository = error "Meta.Docker.mk_deploy_or_error: repository cannot be empty"
    | otherwise = MkDeploy tag repository

docker :: [Os.Arg] -> IO ()
docker args = Os.call "docker" args

-- | Run @docker build@.
build :: Deploy -> IO ()
build MkDeploy{..} = docker ["build", "--tag", _tag, "."]

-- | Run @docker run@.
run :: Deploy -> IO ()
run MkDeploy{..} = docker ["run", "--rm", "--interactive", "--tty", _tag]

push :: Deploy -> IO ()
push MkDeploy{..} = do
    docker ["tag", _tag, _repository]
    docker ["push", _repository]
