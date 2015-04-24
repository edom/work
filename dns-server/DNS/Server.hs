module DNS.Server
(
    getQuestion
    -- * Replying
    , applyReply
    , noError
    , servFail
    , Reply
    -- * Reply manipulation
    , answerMinTtl
    , answer
    , authority
)
where

import Control.Category ((.), (>>>))
import Control.Monad
import Prelude hiding ((.))

import qualified Network.DNS as D

import qualified DNS.Lens.Inside as L
import Lens.Inside

data Reply
    = MkReply
    {
        _rcode :: D.RCODE
        , _answer :: [D.RR D.RDATA]
        , _authority :: [D.RR D.RDATA]
    }
    deriving (Show)

emptyReply :: Reply
emptyReply = MkReply
    {
        _rcode = D.NoErr
        , _answer = []
        , _authority = []
    }

applyReply :: Reply -> D.DNSFormat -> D.DNSFormat
applyReply r =
    respond
    >>> L.answer =: an
    >>> L.authority =: ar
    >>> L.header =$
            (
            L.anCount =: length an
            >>> L.arCount =: length ar
            >>> L.flags =$ L.rcode =: _rcode r
            )
    where
        an = _answer r
        ar = _authority r

-- NXDOMAIN not implemented

{- |
Transform the packet into a response.
-}
respond :: D.DNSFormat -> D.DNSFormat
respond =
    L.header . L.flags =$
        (L.qOrR =: D.QR_Response
        >>> L.authAnswer =: False
        >>> L.recAvailable =: False)

noError :: Reply
noError = emptyReply { _rcode = D.NoErr }

servFail :: Reply
servFail = emptyReply { _rcode = D.ServFail }

-- | Ensure that each answer has a TTL not lower than the given minimum.
answerMinTtl :: Int -> Reply -> Reply
answerMinTtl minTtl = answer =<$> L.ttl =: minTtl

answer :: Inside [D.RR D.RDATA] Reply
answer = mkWithSet _answer (\ x y -> y { _answer = x })

authority :: Inside [D.RR D.RDATA] Reply
authority = mkWithSet _authority (\ x y -> y { _authority = x })

{- |
<http://maradns.samiam.org/multiple.qdcount.html Only support queries with QDCount = 1>.
-}
getQuestion :: D.DNSFormat -> Either String D.Question
getQuestion request = do
    let
        qr = request ^. L.header . L.flags . L.qOrR
        questions_ = request ^. L.question
    unless (qr == D.QR_Query) $ Left "expecting a query"
    case questions_ of
        question_ : [] -> Right question_
        _ -> Left "we only support QDCount = 1"
