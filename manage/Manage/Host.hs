{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Manage.Host where

import Manage.Cast
import System.Cmd
import System.Exit

data Host = Host { address :: String }
            deriving (Eq, Show)

data TcpConnection = TcpConnection { host :: Host, port :: Int }
    deriving (Show, Eq)

localhost :: Host
localhost = Host { address = "127.0.0.1" }

-- | Each instance of this class can be a destination
-- for ICMP echo request packets.
class Ping p where
    -- | Get the ping address.
    pingAddress :: p -> String

instance Ping String where
    pingAddress = id

instance Ping Host where
    pingAddress = address

-- | Send one ICMP echo request packet to @'pingAddress' p@.
ping :: (Ping p) => p -> IO ExitCode
ping p =
    rawSystem "ping" ["-c", "1", pingAddress p]

class Mongo m where
    mongoAddress :: m -> String

    -- | Open a mongo shell.
    --
    -- You must have a directory containing the @mongo@ executable
    -- in your @PATH@.
    mongoShell :: m -> IO ExitCode
    mongoShell m = rawSystem "mongo" [mongoAddress m]

instance Mongo String where
    mongoAddress = id

instance Mongo Host where
    mongoAddress = address
