module DNS.Server
(
    getQuestion
    -- * Responding
    , respond
    , noError
    , servFail
    -- * Manipulation
    , answerMinTtl
    , setAnswer
    , setAuthority
)
where

import Control.Category ((.), (>>>))
import Control.Monad
import Prelude hiding ((.))

import qualified Network.DNS as D

import DNS.Lens.Inside
import Lens.Inside

-- NXDOMAIN not implemented

{- |
Transform the packet into a response.
-}
respond :: D.DNSFormat -> D.DNSFormat
respond =
    header . flags =$
        (qOrR =: D.QR_Response
        >>> authAnswer =: False
        >>> recAvailable =: False)

noError :: D.DNSFormat -> D.DNSFormat
noError = respond >>> header. flags . rcode =: D.NoErr

{- |
Transform the packet into a Server Failure response packet.
-}
servFail :: D.DNSFormat -> D.DNSFormat
servFail = respond >>> header . flags . rcode =: D.ServFail

-- | Ensure that each answer has a TTL not lower than the given minimum.
answerMinTtl :: Int -> D.DNSFormat -> D.DNSFormat
answerMinTtl minTtl = answer =<$> ttl =: minTtl

{- |
<http://maradns.samiam.org/multiple.qdcount.html Only support queries with QDCount = 1>.
-}
getQuestion :: D.DNSFormat -> Either String D.Question
getQuestion request = do
    let
        qr = request ^. header . flags . qOrR
        questions_ = request ^. question
    unless (qr == D.QR_Query) $ Left "expecting a query"
    case questions_ of
        question_ : [] -> Right question_
        _ -> Left "we only support QDCount = 1"

setAnswer :: [D.RR D.RDATA] -> D.DNSFormat -> D.DNSFormat
setAnswer recs =
    answer =: recs
    >>> header . anCount =: length recs

setAuthority :: [D.RR D.RDATA] -> D.DNSFormat -> D.DNSFormat
setAuthority recs =
    authority =: recs
    >>> header . arCount =: length recs
