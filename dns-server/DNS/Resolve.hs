{-# LANGUAGE OverloadedStrings #-}
module DNS.Resolve
(
    -- * Resolvers
    Resolver
    , static
    -- * Trivial resolvers
    , servFail
    , nameError
    -- * Making records
    , a
    -- * Resolver combinators
    , onceSocket
    -- * Types
    , Fqdn
    , Record
    , Ip4String
)
where

import Control.Applicative
import qualified Text.Read as R

import qualified Data.ByteString.Char8 as BSC

import qualified Network.DNS as D

import qualified Network.Socket as NS

import qualified DNS.Error as E
import qualified DNS.Server as S
import qualified DNS.Socket as DS

import Lens.Inside

type Resolver m = D.Question -> m S.Reply

-- | Always gives SERVFAIL.
servFail :: (Applicative m) => Resolver m
servFail = const $ pure S.servFail

-- | Always gives NXDOMAIN.
nameError :: (Applicative m) => Resolver m
nameError = const $ pure S.nameError

-- | Use the resolver to answer queries from the socket once.
onceSocket :: NS.Socket -> Resolver IO -> IO ()
onceSocket socket logic = do
    packet <- DS.recvFrom socket
    let payload = DS._payload packet
    question <- E.checkEither $ S.getQuestion payload
    reply <- logic question
    DS.sendTo socket packet { DS._payload = S.applyReply reply payload }

-- Zonemap = Map String [Record]

-- | Fully qualified domain name.
type Fqdn = D.Domain

-- | DNS resource record.
type Record = D.RR D.RDATA

{- |
Resolve questions using the given list of resource records.
-}
static :: (Applicative m) => [Record] -> Resolver m
static records question =
    case answer of
        [] -> pure S.nameError
        _ -> pure $
            ($ S.noError) $
                S.answer =: answer
    where
        qname = D.qname question
        qtype = D.qtype question
        answer = filter (\ r -> D.rrname r == qname && D.rrtype r == qtype) records

-- | Example: \"192.168.0.1\".
type Ip4String = String

a :: Fqdn -> Ip4String -> Either String Record
a fqdn ip4string =
    either
        (\ msg -> Left $ "a " ++ BSC.unpack fqdn ++ " " ++ ip4string ++ ": " ++ msg)
        (\ addr -> Right $ D.ResourceRecord
            {
                D.rrname = fqdn
                , D.rrtype = D.A
                , D.rrttl = 0
                , D.rdlen = 4
                , D.rdata = D.RD_A addr
            }
        )
        (R.readEither ip4string)
