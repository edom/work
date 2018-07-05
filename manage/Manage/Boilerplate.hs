{-# LANGUAGE MultiParamTypeClasses #-}

module Manage.Boilerplate where

import Manage.Cast
import Manage.Host
import Manage.SSH

instance Cast TcpConnection Host where
    cast = host

class GetHostAddress h where
    getHostAddress :: h -> String

instance GetHostAddress TcpConnection where
    getHostAddress (TcpConnection h _) = address h

class SetHostAddress a where
    setHostAddress :: String -> a -> a

instance SetHostAddress Host where
    setHostAddress a h = h { address = a }

instance SetHostAddress TcpConnection where
    setHostAddress a c = c { host = setHostAddress a (host c) }

instance SetHostAddress Connection where
    setHostAddress a c = c { connection = setHostAddress a (connection c) }

class GetPort p where
    getPort :: p -> Int

instance GetPort TcpConnection where
    getPort (TcpConnection _ p) = p

instance GetHostAddress Connection where
    getHostAddress = getHostAddress . connection

instance GetPort Connection where
    getPort = getPort . connection
