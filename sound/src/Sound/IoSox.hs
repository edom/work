module Sound.IoSox
(
    -- * Input from file
    rvreadfilewav
    , rlreadfilewav
    -- * Format conversion
    , conwavau
)
where

import Sound.InfList
import Sound.IoPtr
import Sound.IoFail
import Sound.StreamVector
import Sound.Time

import qualified System.Exit as Ex

import qualified Data.ByteString as Bs

import qualified Data.Vector.Unboxed as Vu

import qualified System.Process as P

{- |
Convert WAV to AU.

This may not work correctly on Windows.
This may not work correctly on Haskell implementations other than GHC.
(This may hang.)

This requires <http://sox.sourceforge.net/ sox> to be in PATH.
You can install it on Ubuntu 12.04 like this:

@
sudo apt-get install sox
@
-}
conwavau :: Bs.ByteString -> IO Bs.ByteString
conwavau wav_ = do
    (Just i, Just o, _, p) <- P.createProcess (P.proc sox args)
        {
            P.cwd = Just "/tmp"
            , P.std_in = P.CreatePipe
            , P.std_out = P.CreatePipe
            , P.std_err = P.Inherit
        }
    let
        loop wav au = do
            wavTail <- Bs.hPutNonBlocking i wav
            auTail <- Bs.hGetNonBlocking o 1048576
            me <- P.getProcessExitCode p
            case me of
                Just Ex.ExitSuccess -> return au
                Just e -> ioError . userError $ show e
                _ -> loop wavTail (Bs.append au auTail)
    loop wav_ Bs.empty
    where
        sox = "sox"
        args = ["-", "--type", "au", "--encoding", "floating-point", "--bits", "64", "-"]

rvreadfilewav :: FilePath -> IO (Rated (Vu.Vector Double))
rvreadfilewav path =
    slurp path
    >>= conwavau
    >>= return . rvreadau

rlreadfilewav :: FilePath -> IO (Rated (L Double))
rlreadfilewav = fmap (rlfromrv 0) . rvreadfilewav
