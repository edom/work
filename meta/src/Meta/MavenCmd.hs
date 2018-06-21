module Meta.MavenCmd where

import qualified System.Exit as Exit

import qualified System.Process as Process

{- |
@mvn clean compile@
-}
recompile :: FilePath -> IO ()
recompile dir = do
    let spec = (Process.proc "mvn" ["clean", "compile"]) { Process.cwd = Just dir }
    Process.withCreateProcess spec $ \ _mIn _mOut _mErr handle -> do
        code <- Process.waitForProcess handle
        Exit.exitWith code
    return ()
