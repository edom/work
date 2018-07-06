module Manage.SSH (
    -- * Describing connections
    Connection(Connection, connection, user, identityFile, forcePty, switches),
    defaultSshConnection,
    SSH(sshConnection),
    -- * Interactive shell
    ssh,
    loginSpec,
    -- * Remote fragment execution
    remoteSystem,
    remoteSystemFile,
    elevatedRemoteSystem,
) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Manage.Host
import System.Cmd
import System.Exit
import System.IO

-- | This data type describes a secure shell connection.
data Connection
    = Connection {

        -- | @address host m@ is the IP address of to which SSH should connect
        -- in order to connect to the machine.
        --
        -- Do not confuse this with the IP address
        -- assigned to a network interface in the machine.
        --
        -- @port host m@
        -- is the port on which an SSH daemon is listening at the 'address'.
        connection :: TcpConnection,

        -- | The user for logging in with 'ssh' to the machine.
        user :: String,

        -- | Path of the private key file for 'ssh'
        -- logging in to the machine.
        --
        -- This is a path in the machine
        -- that evaluates this function.
        identityFile :: Maybe FilePath,

        -- | Force pseudo-tty allocation (@-t@ switch).
        forcePty :: Bool,

        -- | Other 'ssh' command-line arguments.
        switches :: [String]
    }
    deriving (Eq, Show)

class SSH s where
    sshConnection :: s -> Connection

instance SSH Connection where
    sshConnection = id

instance SSH Host where
    sshConnection h =
        defaultSshConnection {
            connection = (connection defaultSshConnection) { host = h }
        }

instance Ping Connection where
    pingAddress = pingAddress . host . connection

instance Mongo Connection where
    mongoAddress = mongoAddress . host . connection

-- | The default connection descriptor having sensible defaults.
defaultSshConnection =
    Connection {
        connection = TcpConnection { host = localhost, port = 22 },
        user = "ubuntu",
        identityFile = Nothing,
        forcePty = False,
        switches = []
    }

-- FIXME
whenDebug :: IO () -> IO ()
whenDebug act =
    when True act

printWhenDebug :: String -> IO ()
printWhenDebug =
    whenDebug . putStrLn

-- | Execute a remote command using 'ssh' on the machine.
--
-- This is like 'System.Cmd.system' but uses SSH.
remoteSystem :: (SSH c) => c -> [String] -> IO ExitCode
remoteSystem c cmd = do
    printWhenDebug $ show $ "ssh " : args
    rawSystem "ssh" args
    where
        con = sshConnection c
        args =
            concat [
                ["-p", show $ port $ connection con],

                -- FIXME This creates a security hole. What's the correct way to do this?
                ["-o", "StrictHostKeyChecking=no"],
                ["-o", "UserKnownHostsFile=/dev/null"],

                fromJust $
                    ((\ a -> ["-i",a]) <$> identityFile con) `mplus` Just [],
                if forcePty con then ["-t"] else [],
                switches con,
                [loginSpec con],
                [unwords cmd]
            ]

-- | Pass the entire file content to 'remoteSystem'.
remoteSystemFile :: Connection -> FilePath -> IO ExitCode
remoteSystemFile con path =
    withFile path ReadMode $
        (remoteSystem con . (: []) =<<) . hGetContents

-- | Remotely execute the bash fragment with elevated privileges
-- (that is as root).
elevatedRemoteSystem
    :: Connection   -- ^ Connection descriptor.
    -> String       -- ^ Bash fragment.
    -> IO ExitCode
elevatedRemoteSystem con str =
    withFile "elevate.sh" ReadMode $
        (remoteSystem con . (: []) . (++ str) =<<) . hGetContents

-- | Login spec is @user\@address@.
loginSpec :: Connection -> String
loginSpec con =
    concat [user con, "@", address $ host $ connection con]

-- | Establish the described interactive secure shell connection.
ssh :: (SSH c) => c -> IO ExitCode
ssh con =
    remoteSystem con []
