module Meta.Machine (
    Machine
    , empty
    , localhost
    -- * IP address
    , Addr
    , Subnet
    , Ip
    , Tcp
    -- ** Address constructors
    , self
    , neigh
    , world
) where

import qualified Meta.Prelude as P

import qualified Data.Word as W

import qualified Meta.List as L

data Machine = MkMachine {
        _addresses :: [Addr ()]
    } deriving (Read, Show)

empty :: Machine
empty = MkMachine {
        _addresses = []
    }

localhost :: Machine
localhost = empty {
        _addresses = [self none (ip_ "127.0.0.1") ()]
    }

ip :: String -> Maybe Ip
ip str = do
    [Just a, Just b, Just c, Just d] <- return $ map read_oct str_octs
    return $ MkIp a b c d
    where
        read_oct :: String -> Maybe W.Word8
        read_oct = P.readMaybe
        str_octs = L.splitOn "." str

ip_ :: String -> Ip
ip_ str = case ip str of
    Nothing -> error $ "Invalid IP address string: " ++ str
    Just x -> x

none :: Addr a
none = None

-- | Address reachable only from the machine.
self :: Addr () -> Ip -> a -> Addr a
self mach = MkAddr (Self mach)

-- | Address reachable from the machine's neighborhoods (local area networks).
neigh :: Subnet -> Ip -> a -> Addr a
neigh sub = MkAddr (Neigh sub)

-- | Address reachable from the Internet.
world :: Ip -> a -> Addr a
world = MkAddr World

data Ip = MkIp W.Word8 W.Word8 W.Word8 W.Word8
    deriving (Read, Show)

type Tcp = W.Word16

{- |
@a@ represents a protocol on the Internet Protocol (IP).

Let @a = '()'@ for no protocol.
-}
data Addr a
    = None
    | MkAddr Pov Ip a
    deriving (Read, Show)

-- | Point of view.
data Pov
    = Self (Addr ())
    | Neigh Subnet
    | World
    deriving (Read, Show)

-- | Number of bits in host address.
type Slash = Int

data Subnet = MkSubnet Ip Slash
    deriving (Read, Show)
