module Storage where

{-
At the lowest level we have bytes and random-access byte-granular storage.

We can put any bytes into the storage.

To retrieve the entry, we need the storage position, and the size of the entry.
-}

import qualified Foreign as F
import qualified System.IO as I

data Segment
    = Mk_segment
    {
        seg_beg :: Offset -- ^ inclusive
        , seg_end :: Offset -- ^ exclusive
    }
    deriving (Read, Show, Eq)

type Asc_list a = [a]

seg_is_empty :: Segment -> Bool
seg_is_empty s = not (seg_beg s < seg_end s)

seg_intersect :: Segment -> Segment -> Segment
seg_intersect a b =
    Mk_segment (max abeg bbeg) (min aend bend)
    where
        abeg = seg_beg a
        aend = seg_end a
        bbeg = seg_beg b
        bend = seg_end b

{-
subtract :: Segment -> Asc_list Segment -> Asc_list Segment
subtract _ [] = []
subtract used (free : frees) | seg_is_empty ints = free : subtract used frees
subtract used (free : frees) =
    where
        ints = seg_intersect used free
-}

-- These types must be signed integral types.

type Offset = Int
type Capacity = Int
type Count = Int
type Size = Int

data Buffer
    = Mk_buffer
    {
        buf_cap :: Capacity
        , buf_ptr :: F.ForeignPtr F.Word8
    }

buf_new :: Capacity -> IO Buffer
buf_new cap = Mk_buffer cap <$> F.mallocForeignPtrBytes cap

buf_write :: [F.Word8] -> Buffer -> IO ()
buf_write bytelist buf =
    F.withForeignPtr (buf_ptr buf) $ \ ptr ->
        F.pokeArray ptr bytelist

data Byte_store
    = Mk_byte_store
    {
        bs_read :: Buffer -> IO Count
        , bs_write :: Buffer -> IO ()
        , bs_close :: IO ()
    }

data Indexed
    = Mk_indexed
    {
        i_bs_index :: Byte_store
        , i_bs_data :: Byte_store
    }

data Index_entry
    = Mk_index_entry
    {
        ie_offset :: Count -- ^ file address of first byte of entry
        , ie_size :: Count -- ^ number of bytes in entry
    }

data Key_value_store
    = Mk_key_value_store
    {
        kvs_index :: Indexed
    }

data Marshal o
    = Mk_marshal
    {
        m_read :: Buffer -> IO o
        , m_write :: o -> IO ()
    }

file :: FilePath -> IO Byte_store
file path = do
    handle <- I.openBinaryFile path I.ReadWriteMode
    let
        rd buf = do
            F.withForeignPtr (buf_ptr buf) $ \ ptr ->
                I.hGetBuf handle ptr (buf_cap buf)
        wr buf = do
            F.withForeignPtr (buf_ptr buf) $ \ ptr ->
                I.hPutBuf handle ptr (buf_cap buf)
        close = I.hClose handle
    return $ Mk_byte_store rd wr close

test :: IO ()
test = do
    buf <- buf_new 4096
    buf_write (take 4096 $ [0xde,0xad,0xbe,0xef] ++ repeat 0) buf
    sto <- file "/tmp/testdata"
    bs_write sto buf
    bs_close sto
    return ()
