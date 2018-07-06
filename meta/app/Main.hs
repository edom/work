module Main where

import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Manage

data Machine
    = Test
    | Bk
    deriving (Enum, Bounded, Eq, Show)

instance SSH Machine where
    sshConnection = mcon

instance Ping Machine where
    pingAddress = pingAddress . mcon

instance Mongo Machine where
    mongoAddress = mongoAddress . mcon

machines :: [Machine]
machines = enumFrom minBound

mcon :: Machine -> Connection
mcon x =
    case x of
        Test -> setHostAddress "127.0.0.1" con
        Bk -> con {
                connection = (connection (mcon Test)) { port = 2022 }
            }
    where
        con = defaultSshConnection {
            connection = (connection defaultSshConnection) { port = 22 },
            identityFile = Just "/home/erik/.ssh/id_rsa",
            user = "ubuntu"
        }

lo = defaultSshConnection {
    user = "erik",
    identityFile = Just $ concat ["/home/", user lo, "/.ssh/id_rsa"]
    }

rule = Rule "nat" "PREROUTING"
    [InputInterface "eth0", Tcp [DestinationPort 2022]]
    (DNAT "10.0.1.8:22")

setupIptables =
    remoteSystem Test $ [sudo [] ("iptables" : parseRule "-A" rule)]

teardownIptables =
    remoteSystem Test $ [sudo [] ("iptables" : parseRule "-D" rule)]

forMC_ :: [a] -> (a -> IO ()) -> IO ()
forMC_ xs a = forM_ xs (forkIO . a)

diskSpaces = do
    forM_ machines (\ m -> remoteSystem m ["df -h"] >> return ())

uptime ms = do
    forM_ ms (\ m -> remoteSystem m ["uptime"] >> return ())

data Rsync =
    Rsync {
        rsyncConnection :: Connection,
        sourcePaths :: [FilePath],
        dstPath :: FilePath
    }
    deriving (Eq, Show)

instance Command Rsync where
    buildCommand r =
        "rsync -a -v --itemize-changes " ++ unwords (map esc $ sourcePaths r) ++
            -- FIXME This might create a security hole. What's the proper way to do this?
            " --rsh=\"ssh -t -o StrictHostKeyChecking=no" ++
            " -o UserKnownHostsFile=/dev/null" ++
            " -p " ++ esc (show (port (connection con))) ++
            " -i " ++ esc (fromJust (identityFile con)) ++ "\" " ++
            esc (loginSpec con ++ ":" ++ dstPath r)
        where
            con = rsyncConnection r

dir = "/home/ubuntu/backup"

rsync_spec =
    Rsync {
        rsyncConnection = (mcon Bk) { identityFile = Just "/home/erik/.ssh/id_rsa" },
        sourcePaths = ["/var/log"],
        dstPath = dir
    }

backup ms = do
    remoteSystem Bk $ [cmdBk]
    forM ms (\ m -> fmap ((,) m) $ remoteSystem (mconf m) [cmdSubj m])
    where
        u = user (mcon Bk)
        mname = map toLower . show
        mcon' m = (mcon m)
            { identityFile = Just "/home/erik/.ssh/id_rsa" }
        mconf m = (mcon m) { forcePty = True }
        mtarget m = dstPath rsync_spec ++ "/" ++ mname m
        cmdBk = unlines $ [
                mkdirr $ map mtarget ms
                -- sudo [] [chown ["-R"] u (Just u) [dir]]
            ]
        cmdSubj m =
            sudo [] [buildCommand (rsync_spec { dstPath = mtarget m })]

test_remote_sudo = elevatedRemoteSystem (mcon Test) "id -u"

test_ping = mapM ping machines

main :: IO ()
main = return ()
