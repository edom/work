-- | This extends "Data.Serialize" with JVM-specific primitives.
module Meta.JvmSer where

import Prelude (seq)
import Meta.Prelude

import qualified Data.ByteString as Bs
import qualified Data.ByteString.UTF8 as Bu
import qualified Data.Serialize as Se
import qualified Text.Parsec as P

import qualified Meta.JvmAccess as A
import qualified Meta.JvmCls as Cls
import qualified Meta.JvmClsAtr as At
import qualified Meta.JvmClsConst as K
import qualified Meta.JvmConstPool as C
import qualified Meta.JvmType as T

type Get a = Se.Get a

runGet :: Se.Get a -> ByteString -> EitherString a
runGet parser input = either fail return (as_either $ Se.runGet parser input)
    where
        as_either :: Either a b -> Either a b
        as_either = id

-- * Big-endian primitives

-- | Alias for 'Se.getWord8'.
u1 :: Se.Get Word8
u1 = Se.getWord8

-- | Alias for 'Se.getWord16be'.
u2 :: Se.Get Word16
u2 = Se.getWord16be

-- | Alias for 'Se.getWord32be'.
u4 :: Se.Get Word32
u4 = Se.getWord32be

s4 :: Se.Get Int32
s4 = Se.getInt32be

s8 :: Se.Get Int64
s8 = Se.getInt64be

f4 :: Se.Get Float
f4 = Se.getFloat32be

f8 :: Se.Get Double
f8 = Se.getFloat64be

-- * Array

-- | @array_of body@ parses 16-bit big-endian word @count@ followed by @count@ instances of @body@.
array_of :: Se.Get a -> Se.Get [a]
array_of body = do
    count <- fromIntegral <$> Se.getWord16be
    replicateM count body

-- * String

-- | The parameter is usually 'u2' or 'u4'.
g_string :: (Integral a) => Se.Get a -> Se.Get Bs.ByteString
g_string get_length =
    get_length >>= Se.getByteString . fromIntegral

-- * Method signature

{- |
The input is a UTF-8 bytestring.
-}
parse_field_type :: Bs.ByteString -> EitherString T.Type
parse_field_type =
    either (fail . show) return . P.parse field_type "" . Bu.toString

{- |
The input is a UTF-8 bytestring.
-}
parse_method_type :: Bs.ByteString -> EitherString T.Signature
parse_method_type =
    either (fail . show) return . P.parse method_type "" . Bu.toString

field_type :: Parser T.Type
field_type =
    T.Byte <$ P.char 'B'
    <|> T.Char <$ P.char 'C'
    <|> T.Double <$ P.char 'D'
    <|> T.Float <$ P.char 'F'
    <|> T.Int <$ P.char 'I'
    <|> T.Long <$ P.char 'J'
    <|> T.Instance <$> (Bu.fromString <$> (P.char 'L' *> P.manyTill P.anyChar (P.char ';')))
    <|> T.Short <$ P.char 'S'
    <|> T.Bool <$ P.char 'Z'
    <|> T.Array <$ P.char '[' <*> field_type

method_type :: Parser T.Signature
method_type =
    T.Mk_signature
    <$> (P.char '(' *> P.many field_type <* P.char ')')
    <*> return_type
    where
        return_type =
            field_type
            <|> T.Void <$ P.char 'V'

type Parser a = P.Parsec String () a

-- * Parsers

-- ** Attribute

-- | An 'At.Attribute' in a class file.
-- Use @'array_of' attribute@ for a list of attributes.
attribute :: Get (At.Attribute A.PIndex A.ByteString)
attribute = At.make <$> u2 <*> g_string u4

-- * Parsing

-- ** Code

{- |
The input is the attribute content.
It does not include the 6-byte attribute header (name and size).
-}
parse_code_attr_content :: ByteString -> EitherString K.Code
parse_code_attr_content =
    runGet grammar
    where
        grammar = do
            max_stack <- u2
            max_local <- u2
            code <- g_string u4 -- XXX can be too big
            handlers <- do
                count <- fromIntegral <$> u2
                replicateM count $ K.Mk_handler <$> u2 <*> u2 <*> u2 <*> u2
            attrs <- array_of attribute
            return $ K.Mk_code max_stack max_local code handlers attrs

-- * Parsing a class file

-- | A convenience function that does IO and calls 'parse_class'.
parse_class_file :: (MonadIO m) => FilePath -> m (EitherString Cls.Class_file)
parse_class_file path = parse_class path <$> slurp path

-- | Parse the class, calling 'fail' on error.
parse_class :: (Monad m) => FilePath -> ByteString -> m Cls.Class_file
parse_class path =
    either fail return . runGet grammar
    where
        grammar = do
            magic <- u4
            unless (magic == 0xcafebabe) $ fail "bad magic"
            minor <- u2
            major <- u2
            pool <- g_pool
            access <- u2
            this <- u2
            super <- u2
            ifaces <- array_of u2
            fields <- array_of $ K.Mk_field_info <$> u2 <*> u2 <*> u2 <*> array_of attribute
            methods <- array_of $ K.Mk_method_info <$> u2 <*> u2 <*> u2 <*> array_of attribute
            attrs <- array_of attribute
            return $ Cls.Mk_class minor major pool access this super ifaces fields methods attrs
        -- "In retrospect, making 8-byte constants take two constant pool entries was a poor choice."
        -- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.5
        g_pool = do
            n <- fromIntegral <$> u2
            let
                count = n - 1
                loop result i | i >= count = return (reverse result)
                loop result i = do
                    tag <- u1
                    let
                        mem_index = i + 1
                        one = fmap (\ x -> [x])
                        two = fmap (\ x -> [K.unused, x]) -- reversed
                        -- K.unused is not stored on disk, but is present on memory.
                    new_entries <- case tag of
                        1 -> one $ K.utf8 <$> g_string u2
                        3 -> one $ K.integer <$> s4
                        4 -> one $ K.float <$> f4
                        5 -> two $ K.long <$> s8
                        6 -> two $ K.double <$> f8
                        7 -> one $ K.class_ <$> u2
                        8 -> one $ K.string <$> u2
                        9 -> one $ K.fieldref <$> u2 <*> u2
                        10 -> one $ K.methodref <$> u2 <*> u2
                        11 -> one $ K.interfacemethodref <$> u2 <*> u2
                        12 -> one $ K.nameandtype <$> u2 <*> u2
                        _ -> fail $ path ++ ": constant pool entry #" ++ show mem_index ++ " (D" ++ show i ++ ") has invalid tag: " ++ show tag
                    let
                        new_i = i + length new_entries
                        -- For debugging, uncomment the first definition of 'trace' and comment the second one.
                        -- trace u = Debug.Trace.trace ("#" ++ show mem_index ++ " (D" ++ show i ++ "): " ++ show new_entries) u
                        trace = id
                    trace $ new_i `seq` loop (new_entries ++ result) new_i
            loop [] 0

{- |
The 'FilePath' argument is used for error reporting.
-}
parse_class_0 :: (Monad m) => FilePath -> ByteString -> m (C.Class C.Raw)
parse_class_0 path =
    either fail return . Se.runGet grammar
    where
        grammar = do
            magic <- u4
            unless (magic == 0xcafebabe) $ fail "bad magic"
            minor <- u2
            major <- u2
            pool <- fromList <$> g_pool_0
            access <- u2
            this <- u2
            super <- u2
            ifaces <- array_of u2
            fields <- array_of $ C.Mk_field_info <$> u2 <*> u2 <*> u2 <*> g_attributes_0
            methods <- array_of $ C.Mk_method_info <$> u2 <*> u2 <*> u2 <*> g_attributes_0
            attrs <- g_attributes_0
            return $ C.Mk_class minor major pool access this super ifaces fields methods attrs
        -- "In retrospect, making 8-byte constants take two constant pool entries was a poor choice."
        -- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.5
        g_pool_0 = do
            n <- fromIntegral <$> u2
            let
                count = n - 1
                loop result i | i >= count = return (reverse result)
                loop result i = do
                    tag <- u1
                    let
                        mem_index = i + 1
                        one = fmap (\ x -> [x])
                        two = fmap (\ x -> [C.Unused, x]) -- reversed
                        -- C.Unused is not stored on disk, but is present on memory.
                    new_entries <- case tag of
                        1 -> one $ C.Utf8 <$> g_string u2
                        3 -> one $ C.Integer <$> s4
                        4 -> one $ C.Float <$> f4
                        5 -> two $ C.Long <$> s8
                        6 -> two $ C.Double <$> f8
                        7 -> one $ C.EClass <$> u2
                        8 -> one $ C.String <$> u2
                        9 -> one $ C.Fieldref <$> u2 <*> u2
                        10 -> one $ C.Methodref <$> u2 <*> u2
                        11 -> one $ C.Interfacemethodref <$> u2 <*> u2
                        12 -> one $ C.Nameandtype <$> u2 <*> u2
                        _ -> fail $ path ++ ": constant pool entry #" ++ show mem_index ++ " (D" ++ show i ++ ") has invalid tag: " ++ show tag
                    let
                        new_i = i + length new_entries
                        -- For debugging, uncomment the first definition of 'trace' and comment the second one.
                        -- trace u = Debug.Trace.trace ("#" ++ show mem_index ++ " (D" ++ show i ++ "): " ++ show new_entries) u
                        trace = id
                    trace $ new_i `seq` loop (new_entries ++ result) new_i
            loop [] 0
        g_attributes_0 :: Get [C.Attribute C.Index]
        g_attributes_0 =
            array_of $ C.Mk_attribute <$> u2 <*> g_string u4
