{-# LANGUAGE OverloadedStrings #-}
module DNS.Resolve
(
    -- * Resolvers
    Resolver
    , mkResolver
    , mapResolver
    -- * Static
    , static
    -- * Trivial resolvers
    , servFail
    , nameError
    -- * Making records
    , a
    , ns
    -- * Running
    , runResolver
    , onceSocket
    -- * Types
    , Fqdn
    , Record
)
where

import Control.Applicative
import Data.Monoid
import Text.Printf
import qualified Data.Time as T
import qualified System.IO.Error as IE
import qualified Text.Read as R

import qualified Data.ByteString.Char8 as BSC

import qualified Network.DNS as D

import qualified Network.Socket as NS

import qualified DNS.Error as E
import qualified DNS.Server as S
import qualified DNS.Socket as DS

import Lens.Inside

newtype Resolver m = MkResolver { _runResolver :: D.Question -> m S.Reply }

{- |
This works similarly to the 'Monoid' instance of 'S.Reply':
keep trying until 'S.isNoError' is satisfied or the operands are exhausted.
-}
instance (Monad m) => Monoid (Resolver m) where
    mempty = mkResolver $ const $ return mempty
    mappend x y = mkResolver $ \ q -> do
        u <- runResolver x q
        if S.isNoError u
            then return u
            else runResolver y q

mkResolver :: (D.Question -> m S.Reply) -> Resolver m
mkResolver = MkResolver

runResolver :: Resolver m -> D.Question -> m S.Reply
runResolver = _runResolver

mapResolver :: (Functor m) => (S.Reply -> S.Reply) -> Resolver m -> Resolver m
mapResolver f x = mkResolver $ fmap f . runResolver x

-- | Always gives SERVFAIL.
servFail :: (Applicative m) => Resolver m
servFail = mkResolver $ const $ pure S.servFail

-- | Always gives NXDOMAIN.
nameError :: (Applicative m) => Resolver m
nameError = mkResolver $ const $ pure S.nameError

-- | Use the resolver to answer queries from the socket once.
onceSocket :: NS.Socket -> Resolver IO -> IO ()
onceSocket socket resolve = do
    packet <- DS.recvFrom socket
    let payload = DS._payload packet
    question <- E.checkEither $ S.getQuestion payload
    flip IE.catchIOError (\ e -> putStrLn (show question) >> print e) $ do
        (ps, reply) <- timed $ runResolver resolve question
        putStrLn $ showQuestion question ++ printf " %.0f ms" (realToFrac ps * 1000 :: Double) -- debug
        DS.sendTo socket packet { DS._payload = S.applyReply reply payload }
    where
        showQuestion q = show (D.qtype q) ++ " " ++ BSC.unpack (D.qname q)

timed :: IO a -> IO (T.NominalDiffTime, a)
timed action = do
    t <- T.getCurrentTime
    x <- action
    u <- T.getCurrentTime
    let d = T.diffUTCTime u t -- picoseconds
    return (d, x)

-- Zonemap = Map String [Record]

-- | Fully qualified domain name.
type Fqdn = D.Domain

-- | DNS resource record.
type Record = D.RR D.RDATA

{- |
Resolve questions using the given list of resource records.
-}
static :: (Applicative m) => [Record] -> Resolver m
static records = mkResolver resolve
    where
        resolve question =
            case answer of
                [] -> pure S.nameError
                _ -> pure $
                    ($ S.noError) $
                        S.answer =: answer
            where
                qname = D.qname question
                qtype = D.qtype question
                answer = filter (\ r -> D.rrname r == qname && D.rrtype r == qtype) records

a :: Fqdn -> DS.Ip4String -> Either String Record
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

-- | @ns a b@ represents that @b@ is a name server of @a@.
ns :: Fqdn -> Fqdn -> Record
ns fqdn nsfqdn =
    D.ResourceRecord
    {
        D.rrname = fqdn
        , D.rrtype = D.NS
        , D.rrttl = 0
        , D.rdlen = BSC.length nsfqdn
        , D.rdata = D.RD_NS nsfqdn
    }
