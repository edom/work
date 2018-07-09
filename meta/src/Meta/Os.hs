module Meta.Os (
    -- * Process
    module Meta.OsProc
    -- * Command-line arguments
    , Env.getProgName
    , Env.getArgs
    , Env.withProgName
    , Env.withArgs
    -- * Environment
    , Env.getEnv
    -- , Env.lookupEnv -- not in base 4.5
    , Env.getEnvironment
) where

import Meta.OsProc

import qualified System.Environment as Env
