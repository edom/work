{-# LANGUAGE OverloadedStrings #-}
-- | DNS resolution using <http://www.statdns.com/api/ StatDNS REST DNS API>.
module DNS.Web
(
    query
)
where

import Control.Applicative
import Control.Category ((>>>))
import qualified Control.Exception as E
import qualified System.IO.Error as IE
import qualified Text.Read as R

import qualified Data.ByteString.Char8 as BSC

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT

import qualified Network.DNS as D

import qualified Network.HTTP.Client as H

import qualified DNS.Lens.Inside as L
import DNS.Server
import Lens.Inside

checkEither :: Either String a -> IO a
checkEither = either (IE.ioError . IE.userError) return

query :: H.Manager -> D.DNSFormat -> IO D.DNSFormat
query manager request =
    flip IE.catchIOError (\ e -> print e >> return (servFail request)) $
    E.handle (\ e -> putStrLn (showHttpException e) >> return (servFail request)) $ do
        question <- checkEither $ getQuestion request
        object <- restQuery manager question
        -- print object -- debug
        checkEither $ flip JT.parseEither object $ \ x -> do
            an <- x J..: "answer" >>= mapM mkRr
            ar <- (maybe [] id <$> x J..:? "authority") >>= mapM mkRr
            return $
                L.header =$
                    (L.anCount =: length an
                    >>> L.arCount =: length ar
                    >>> L.flags =$ L.rcode =: D.NoErr)
                >>> L.answer =: an
                >>> L.authority =: ar
                $ respond request
    where
        showHttpException :: H.HttpException -> String
        showHttpException = show

mkRr :: J.Object -> JT.Parser (D.RR D.RDATA)
mkRr a = do
    strType <- a J..: "type"
    rtype <- either fail return $ readType strType
    rdata <- a J..: "rdata"
    D.ResourceRecord
        <$> (BSC.pack <$> a J..: "name")
        <*> pure rtype
        <*> a J..: "ttl"
        <*> a J..: "rdlength"
        <*> either fail pure (case rtype of
                D.A -> D.RD_A <$> R.readEither rdata
                D.NS -> return $ D.RD_NS $ BSC.pack rdata
                _ -> Left $ "unknown type:" ++ strType
                )

restQuery :: H.Manager -> D.Question -> IO J.Object
restQuery manager question = do
    urlpart <- either (IE.ioError . IE.userError) return urlpart_
    httpRequest <- H.parseUrl $ "http://api.statdns.com/" ++ BSC.unpack domain ++ "/" ++ urlpart
    H.withResponse httpRequest manager $ \ response ->
        H.brRead (H.responseBody response) >>= checkEither . J.eitherDecodeStrict'
    where
        domain = D.qname question
        qtype = D.qtype question
        urlpart_ = case qtype of
            D.A -> Right "a"
            D.NS -> Right "ns"
            _ -> Left $ "unsupported question type: " ++ show qtype

readType :: String -> Either String D.TYPE
readType x = case x of
    "A" -> return D.A
    "NS" -> return D.NS
    _ -> Left $ "unknown type: " ++ x
