module Meta.Http (
    -- * HTTP
    Url
    , get_ByteString
) where

import Prelude ()
import Meta.Prelude

import qualified Network.HTTP.Client as C

import qualified Meta.ByteString as B

type Url = String

get_ByteString :: Url -> IO B.ByteString
get_ByteString url =
    -- In http-conduit 0.5, 'withManager' was deprecated, and 'closeManager' became a no-operation.
    C.withManager C.defaultManagerSettings $ \ manager -> do
        request <- C.parseRequest url
        response <- C.httpLbs request manager
        return $ B.toStrict $ C.responseBody response
