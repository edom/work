{-# LANGUAGE FlexibleInstances #-}

-- | Build command line string from argument list.
--
-- To execute the resulting string on a remote machine,
-- see 'remoteSystem' and its variants.

module Manage.Command (

    -- * Combinators

    Sequence(Sequence),

    -- * Builders

    Command(buildCommand),

    esc,

    rm,
    sudo,
    aptGet,
    aptInit,
    aptInstall,
    mkdir,
    mkdirr,
    chown,
    rmdir,
    scp,
    removeKnownHost,
) where

import Control.Monad
import Data.Maybe
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Manage.Boilerplate
import Manage.SSH
import Manage.Host
import Text.Printf
import Text.ShellEscape

class Command c where
    buildCommand :: c -> String

instance Command String where
    buildCommand = id

-- | Execute command @a@ and then command @b@.

data Sequence a b = Sequence a b
    deriving (Eq, Show)

instance (Command a, Command b) => Command (Sequence a b) where
    buildCommand (Sequence a b) = buildCommand a ++ "\n" ++ buildCommand b

esc :: String -> String
esc =
    unpack . bytes . (escape :: ByteString -> Bash) . pack

-- | Remotely execute the command with elevated privileges.
sudo :: [String] -> [String] -> String
sudo args cmdl = unwords $ concat [["sudo"], args, ["--"], cmdl]

aptGet :: [String] -> String
aptGet = sudo [] . ("apt-get" :)

-- |
-- @
-- sudo apt-get update
-- @
aptInit :: [String] -> String
aptInit = aptGet . ("update" :)

-- |
-- @
-- sudo apt-get install --no-install--recommends --assume-yes
-- @
aptInstall :: [String] -> String
aptInstall = aptGet . (["--no-install-recommends", "--assume-yes"] ++)

-- | This command is /dangerous/.
rm :: [String] -> String
rm = ("rm -r -f " ++) . unwords

-- | This assumes GNU coreutils.
--
-- @
-- --parents
-- @
--
-- @
-- --mode
-- @

mkdir :: [String] -> String
mkdir = ("mkdir " ++) . unwords

-- | Recursive 'mkdir'.
--
-- This assumes GNU coreutils.

mkdirr :: [String] -> String
mkdirr = mkdir . ("--parents" :)

-- | This assumes GNU coreutils.

chown :: [String] -> String -> Maybe String -> [FilePath] -> String
chown switches owner mgroup paths =
    unwords $ concat [
            ["chown"],
            switches,
            [og],
            paths
        ]
    where
        og = owner ++ fromJust (fmap (":" ++) mgroup `mplus` return "")

rmdir :: [String] -> String
rmdir = ("rmdir " ++) . unwords

-- | Copy the files specified by the paths in @spaths@
-- (which are paths to files in the machine @srcm@)
-- to the path @dpath@ in the machine @dstm@.
--
-- This requires elevated privileges to read files
-- that might not be readable by the login user.

scp :: [String] -> [FilePath] -> Connection -> FilePath -> String
scp switches spaths con dpath =
    unwords $ concat [
            ["scp"],
            switches,
            fromJust $ fmap (\ x -> ["-i",x]) (identityFile con) `mplus` Just [],
            [
                "-r",   -- Recursive. (Warning: scp follows symbolic links.)
                "-p"    -- Preserve times and modes.

            ],

            -- XXX
            ["-o", "StrictHostKeyChecking=no"],
            ["-o", "UserKnownHostsFile=/dev/null"],

            ["-P", show $ getPort con],
            spaths,
            [concat [loginSpec con, ":", dpath]]
        ]

-- | Remove known host in ssh known-hosts file.

removeKnownHost :: (SSH c) => c -> String
removeKnownHost m =
    unwords [
        "ssh-keygen",
        "-R",
        esc $ printf "[%s]:%d" (getHostAddress n) (getPort n)]
    where
        n = sshConnection m
