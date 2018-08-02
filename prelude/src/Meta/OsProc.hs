{- |
Operating system processes.

Every function here throws an exception if the subprocess exits with non-zero exit code.

This wraps the most commonly used parts of these libraries and modules:

* directory: "System.Directory"

* process: "System.Process"
-}
module Meta.OsProc (
    -- * Running subprocesses
    Prog
    , Arg
    , Stdin
    , Stdout
    , call
    , read
    -- * Working directory
    , getcwd
    , setcwd
    , withcwd
) where

import Prelude hiding (read)

import qualified System.Directory as D
import qualified System.Process as P

-- | A path to an executable.
type Prog = String

-- | An argument to pass to an executable.
type Arg = String

-- | Things to write to subprocess standard input.
type Stdin = String

-- | Things read from subprocess standard output.
type Stdout = String

{- |
Start the process, and wait for it to finish.

The subprocess inherits the parent process's streams.

This wraps 'P.callProcess'.
-}
call :: Prog -> [Arg] -> IO ()
call = P.callProcess

{- |
This is 'call' with custom standard input and output.

The subprocess still inherits the parent process's standard error.

This wraps 'P.readProcess'.
-}
read :: Prog -> [Arg] -> Stdin -> IO Stdout
read = P.readProcess

-- | This is 'D.getCurrentDirectory'.
getcwd :: IO FilePath
getcwd = D.getCurrentDirectory

-- | This is 'D.setCurrentDirectory'.
setcwd :: FilePath -> IO ()
setcwd = D.setCurrentDirectory

-- | This is 'D.withCurrentDirectory'.
withcwd :: FilePath -> IO a -> IO a
withcwd = D.withCurrentDirectory
