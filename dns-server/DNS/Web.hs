{-# LANGUAGE OverloadedStrings #-}
-- | DNS resolution using <http://www.statdns.com/api/ StatDNS REST DNS API>.
module DNS.Web
(
    lookupA
)
where

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad
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

-- | Serve an A query.
lookupA :: H.Manager -> D.DNSFormat -> IO D.DNSFormat
lookupA manager request =
    flip IE.catchIOError (\ e -> print e >> return (servFail request)) $
    E.handle (\ e -> putStrLn (showHttpException e) >> return (servFail request)) $ do
        question <- checkEither $ getQuestion request
        let domain = D.qname question
        httpRequest <- H.parseUrl $ "http://api.statdns.com/" ++ BSC.unpack domain ++ "/a"
        H.withResponse httpRequest manager $ \ response -> do
            q <- H.brRead $ H.responseBody response
            object <- checkEither $ J.eitherDecodeStrict' q
            -- print object -- debug
            checkEither $ flip JT.parseEither object $ \ x -> do
                answers <- x J..: "answer"
                questions <- x J..: "question"
                authorities <- maybe [] id <$> x J..:? "authority"
                an <- mapM mkRr answers
                qd <- forM questions $ \ q_ ->
                    D.Question
                        <$> (BSC.pack <$> q_ J..: "name")
                        <*> (q_ J..: "type" >>= either fail return . readType)
                ar <- mapM mkRr authorities
                return $
                    L.header =$
                        (L.qdCount =: length qd
                        >>> L.anCount =: length an
                        >>> L.arCount =: length ar
                        >>> L.flags =$ L.rcode =: D.NoErr)
                    >>> L.question =: qd
                    >>> L.answer =: an
                    >>> L.authority =: ar
                    $ respond request
    where
        showHttpException :: H.HttpException -> String
        showHttpException = show
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
                        ) -- XXX a J..: "class"

readType :: String -> Either String D.TYPE
readType x = case x of
    "A" -> return D.A
    "NS" -> return D.NS
    _ -> Left $ "unknown type: " ++ x
