{-# LANGUAGE BangPatterns #-}

{- |
This follows the <http://www.ccarh.org/courses/253/handout/smf/ standard MIDI file format>.

We want to keep the code under BSD license
but Haskell\'s MIDI package is licensed under GPL
so we cannot use it here.
-}

module Sound.Midi
(
    Smf
    , readsmf
    , smfformat
    , smfntrack
    , smfdivisi
    , Cntl
    , Prog
    , Control(..)
    , Program(..)
    -- * Unknown
    , embed
    , fmsb
    , mhead
    , chunk
)
where

import Control.Applicative (pure, (<*>), (<$>), (<*), (<|>))
import Data.Bits ((.&.))
import Data.Word (Word8)

import Data.Attoparsec.ByteString (anyWord8, word8)

import qualified Control.Applicative as Ap
import qualified Control.Monad as Cm
import qualified Data.Bits as B

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Binary as Ab

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as Bsc

import qualified Data.Serialize as G

readsmf :: Bs.ByteString -> Either String Smf
readsmf = A.parseOnly psmf

embed :: G.Get a -> Bs.ByteString -> G.Get a
embed g s = either fail return $ G.runGet g s

psmf :: A.Parser Smf
psmf = pure MkSmf <*> phead <*> A.many' ptrack

pchunk :: A.Parser Chunk
pchunk = do
    ctype <- A.take 4
    csize <- fmap fromIntegral Ab.anyWord32be
    -- artificial limitation to prevent denial of service
    Cm.unless (csize < maxChunkSize) $ fail "chunk too large"
    cdata <- A.take csize
    return $ MkChunk ctype csize cdata
    where
        maxChunkSize = 1048576

phead :: A.Parser Head
phead = do
    (MkChunk t s b) <- pchunk
    Cm.unless (t == Bsc.pack "MThd") $ fail "expecting MThd"
    Cm.unless (s >= 6) $ fail "MThd chunk payload must have at least 6 bytes"
    either fail return . flip A.parseOnly b $ do
        format <- fmap fromIntegral Ab.anyWord16be
        ntrack <- fmap fromIntegral Ab.anyWord16be
        divisi <- fmap fromIntegral Ab.anyWord16be
        return $ MkHead format ntrack divisi

ptrack :: A.Parser Track
ptrack = do
    (MkChunk t _ b) <- pchunk
    Cm.unless (t == emagic) $ fail "expecting MTrk"
    either fail return $ A.parseOnly (pmtrkevents 0) b
    where
        emagic = Bsc.pack "MTrk"

type Status = Word8

pmtrkevents :: Status -> A.Parser [MtrkEvent]
pmtrkevents runningStatus =
    (
        do
            dtime <- pdtime
            sta <- statusByte <|> pure runningStatus
            fcons
                (MkMtrkEvent dtime <$> pevent sta)
                (pmtrkevents sta)
    )
    <|> pure []
    where
        fcons = Ap.liftA2 (:)
        pdtime = pvarint
        statusByte = A.satisfy (\ b -> B.testBit b 7)

pevent :: Status -> A.Parser Event
pevent sta =
    case sta of
        0xFF ->
            fmap MkMetaEvent pmetaevent
        _ ->
            let
                cha = nib0 sta
            in
                fmap MkSysexEvent psysexevent
                <|> MkMidiEvent cha <$> pmidievent sta

nib0 :: Word8 -> Word8
nib0 = (.&.) 0x0F

nib1 :: Word8 -> Word8
nib1 = (.&.) 0xF0

pmidievent :: Status -> A.Parser MidiEvent
pmidievent sta =
    case nib1 sta of
        0x80 -> pure NoteOff <*> key <*> vel
        0x90 -> pure NoteOn <*> key <*> vel
        0xB0 -> do
            c <- cntl
            ControlChange
                (
                    case c of
                        0x00 -> Bank
                        0x07 -> Volume
                        0x0A -> Pan
                        0x40 -> Damper
                        _ -> UnknownControl c
                )
                <$> cval
        0xC0 ->
            (\ p ->
                ProgramChange
                (
                    case p of
                        0 -> AcousticGrandPiano
                        4 -> ElectricPiano1
                        5 -> ElectricPiano2
                        19 -> ChurchOrgan
                        32 -> AcousticBass
                        48 -> StringEnsemble1
                        49 -> StringEnsemble2
                        _ -> UnknownProgram p
                )
            )
            <$> prog
        _ -> pure (UnknownMidiStatus sta)
    where
        key = anyWord8
        vel = anyWord8
        prog = anyWord8
        cntl = anyWord8
        cval = anyWord8

-- | A 'Word8' whose four most significant bits are equal to the given pattern.
-- This returns the four least significant bits, which should be the channel number.
fmsb :: Word8 -> A.Parser Word8
fmsb expe =
    fmap (0x0F B..&.) $ A.satisfy (\ b -> B.shiftR b 4 == expe)

pmetaevent :: A.Parser MetaEvent
pmetaevent =
    pure CopyrightNotice <* word8 0x02 <*> (pvarint >>= A.take)
    <|> pure TrackName <* word8 0x03 <*> (pvarint >>= A.take)
    <|> pure CuePoint <* word8 0x07 <*> (pvarint >>= A.take)
    <|> pure EndOfTrack <* word8 47 <* word8 0x00
    <|> pure SetTempo <* word8 0x51 <* word8 0x03 <*> anyWord24be
    <|> pure TimeSignature <* word8 0x58 <* word8 0x04 <*> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8
    <|> fmap UnkME anyWord8

anyWord24be :: A.Parser Int
anyWord24be = do
    pure (\ a b c -> B.shiftL a 16 B..|. B.shiftL b 8 B..|. c) <*> w <*> w <*> w
    where
        w = fmap fromIntegral anyWord8

psysexevent :: A.Parser SysexEvent
psysexevent = fail "psysexevent not implemented"

mhead :: G.Get Head
mhead = do
    amagic <- G.getBytes 4
    Cm.unless (amagic == emagic) $ fail "wrong magic; expecting MThd"
    _ <- G.getWord32be -- header length
    format <- fmap fromIntegral G.getWord16be
    ntrack <- fmap fromIntegral G.getWord16be
    divisi <- fmap fromIntegral G.getWord16be
    return $ MkHead format ntrack divisi
    where
        emagic = Bsc.pack "MThd"

-- | Parse a MIDI variable-length integer.
pvarint :: A.Parser Int
pvarint = do
    pref <- A.takeWhile (flip B.testBit 7)
    end <- anyWord8
    return . read_ $ Bs.snoc pref end
    where
        read_ = Bs.foldl (\ v b -> B.shiftL v 7 B..|. fromIntegral (B.clearBit b 7)) 0

data Smf
    = MkSmf
    {
        _smfhead :: Head
        , _smftracks :: [Track]
    }
    deriving (Read, Show)

smfformat :: Smf -> Int
smfformat = _format . _smfhead

smfntrack :: Smf -> Int
smfntrack = _ntrack . _smfhead

-- | Number of ticks per quarter-note.
smfdivisi :: Smf -> Int
smfdivisi = _divisi . _smfhead

data Chunk
    = MkChunk
    {
        _ctype :: !Bs.ByteString
        , _csize :: !Int
        , _cdata :: !Bs.ByteString
    }
    deriving (Read, Show)

data Head
    = MkHead
    {
        _format :: Int
        , _ntrack :: Int
        , _divisi :: Int
    }
    deriving (Read, Show)

type Track = [MtrkEvent]
data MtrkEvent
    = MkMtrkEvent
    {
        _metime :: Int
        , _mevent :: Event
    }
    deriving (Read, Show)

data Event
    = MkMidiEvent Chan MidiEvent
    | MkMetaEvent MetaEvent
    | MkSysexEvent SysexEvent
    deriving (Read, Show)

type Chan = Word8
type MidiNote = Word8
type Velo = Word8
type Prog = Word8
type Cntl = Word8
type Cval = Word8
type MetaEventType = Word8
data MidiEvent
    = NoteOff MidiNote Velo
    | NoteOn MidiNote Velo
    | ProgramChange Program
    | ControlChange Control Cval
    | UnknownMidiStatus Status
    deriving (Read, Show)
data Program
    = AcousticGrandPiano
    | ElectricPiano1 -- rhodes
    | ElectricPiano2 -- chorus
    | ChurchOrgan
    | AcousticBass
    | StringEnsemble1
    | StringEnsemble2
    | UnknownProgram Prog
    deriving (Read, Show)
data Control
    = Bank
    | Volume
    | Pan
    | Damper
    | UnknownControl Cntl
    deriving (Read, Show)
data MetaEvent
    = CopyrightNotice Bs.ByteString
    | CuePoint Bs.ByteString
    | TrackName Bs.ByteString -- can also be Sequence Name
    | EndOfTrack
    | SetTempo Int
    | TimeSignature Word8 Word8 Word8 Word8
    | UnkME MetaEventType
    deriving (Read, Show)
data SysexEvent
    = UnkSE
    deriving (Read, Show)

chunk :: G.Get Chunk
chunk = do
    ctype <- G.getBytes 4
    csize <- fmap fromIntegral G.getWord32be
    -- artificial limitation to prevent denial of service
    Cm.unless (csize < maxChunkSize) $ fail "chunk too large"
    cdata <- G.getBytes csize
    return $ MkChunk ctype csize cdata
    where
        maxChunkSize = 1048576
