module Meta.Machine (
    Machine
    , empty
    , localhost
    , host
    , local
    , public
    , Address
) where

data Machine = MkMachine {
        _addresses :: [Address]
    } deriving (Read, Show)

empty :: Machine
empty = MkMachine {
        _addresses = []
    }

localhost :: Machine
localhost = empty {
        _addresses = [host "127.0.0.1"]
    }

-- | Address reachable only from the machine.
host :: String -> Address
host = MkAddress Host

-- | address reachable from the machine's local area networks.
local :: String -> Address
local = MkAddress Local

-- | address reachable from the Internet.
public :: String -> Address
public = MkAddress Public

data Address
    = MkAddress Scope String
    deriving (Read, Show)

data Scope
    = Host
    | Local
    | Public
    deriving (Read, Show)
