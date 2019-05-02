module DNS.Lens.Inside
(
    -- * Packet
    header
    , question
    , answer
    , authority
    -- * Header
    , flags
    , qdCount
    , anCount
    , arCount
    -- * Flags
    , qOrR
    , authAnswer
    , recAvailable
    , rcode
    -- * Records
    , ttl
)
where

import qualified Network.DNS as D

import Lens.Inside

header :: Inside D.DNSHeader D.DNSFormat
header = mkWithSet D.header (\ x y -> y { D.header = x })

flags :: Inside D.DNSFlags D.DNSHeader
flags = mkWithSet D.flags (\ x y -> y { D.flags = x })

qOrR :: Inside D.QorR D.DNSFlags
qOrR = mkWithSet D.qOrR (\ x y -> y { D.qOrR = x })

authAnswer :: Inside Bool D.DNSFlags
authAnswer = mkWithSet D.authAnswer (\ x y -> y { D.authAnswer = x })

recAvailable :: Inside Bool D.DNSFlags
recAvailable = mkWithSet D.recAvailable (\ x y -> y { D.recAvailable = x })

rcode :: Inside D.RCODE D.DNSFlags
rcode = mkWithSet D.rcode (\ x y -> y { D.rcode = x })

anCount :: Inside Int D.DNSHeader
anCount = mkWithSet D.anCount (\ x y -> y { D.anCount = x })

arCount :: Inside Int D.DNSHeader
arCount = mkWithSet D.arCount (\ x y -> y { D.arCount = x })

qdCount :: Inside Int D.DNSHeader
qdCount = mkWithSet D.qdCount (\ x y -> y { D.qdCount = x })

question :: Inside [D.Question] D.DNSFormat
question = mkWithSet D.question (\ x y -> y { D.question = x })

answer :: Inside [D.RR D.RDATA] D.DNSFormat
answer = mkWithSet D.answer (\ x y -> y { D.answer = x })

authority :: Inside [D.RR D.RDATA] D.DNSFormat
authority = mkWithSet D.authority (\ x y -> y { D.authority = x })

ttl :: Inside Int (D.RR a)
ttl = mkWithSet D.rrttl (\ x y -> y { D.rrttl = x })
