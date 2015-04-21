module DNS.Server
(
    respond
    , servFail
    , getQuestion
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
Transform the packet into a response with no answers.
-}
respond :: D.DNSFormat -> D.DNSFormat
respond =
    header . flags =$
        (qOrR =: D.QR_Response
        >>> authAnswer =: False
        >>> recAvailable =: True)

{- |
Transform the packet into a Server Failure response packet.
-}
servFail :: D.DNSFormat -> D.DNSFormat
servFail = respond >>> header . flags . rcode =: D.ServFail

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
