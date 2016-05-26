{- |
Class file parsing.
-}
module Jvm_io
where

import Control.Applicative
    (
        (<|>)
    )

import qualified Control.Monad as M
import qualified Debug.Trace as Dbg

import Data.Int
    (
        Int32
        , Int64
    )
import Data.Word
    (
        Word8
        , Word16
        , Word32
    )

import Data.ByteString (ByteString)

import Data.Serialize (Get)

import qualified Data.ByteString as Bs

import qualified Data.Serialize as Se

import qualified Text.Parsec as P

import qualified Data.ByteString.UTF8 as Bu

import qualified List as Li

import Jvm_type
    (
        Type(..)
        , Return_type
        , Signature(..)
    )

-- * Parse class files

{- |
The 'FilePath' argument is used for error reporting.
-}
parse_class :: FilePath -> ByteString -> Either String Class
parse_class path =
    Se.runGet grammar
    where
        grammar = do
            magic <- u4
            M.unless (magic == 0xcafebabe) $ fail "bad magic"
            minor <- u2
            major <- u2
            pool <- g_pool
            access <- u2
            this <- u2
            super <- u2
            ifaces <- array_of u2
            fields <- array_of $ Mk_field_info <$> u2 <*> u2 <*> u2 <*> g_attributes
            methods <- array_of $ Mk_method_info <$> u2 <*> u2 <*> u2 <*> g_attributes
            attrs <- g_attributes
            return $ Mk_class minor major pool access this super ifaces fields methods attrs
        f8 = Se.getFloat64be
        f4 = Se.getFloat32be
        s8 = Se.getInt64be
        s4 = Se.getInt32be
        u4 = Se.getWord32be
        u2 = Se.getWord16be
        u1 = Se.getWord8
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
                        two = fmap (\ x -> [P_unused, x]) -- reversed
                        -- P_unused is not stored on disk, but is present on memory.
                    new_entries <- case tag of
                        1 -> one $ P_utf8 <$> g_string u2
                        3 -> one $ P_integer <$> s4
                        4 -> one $ P_float <$> f4
                        5 -> two $ P_long <$> s8
                        6 -> two $ P_double <$> f8
                        7 -> one $ P_class <$> u2
                        8 -> one $ P_string <$> u2
                        9 -> one $ P_fieldref <$> u2 <*> u2
                        10 -> one $ P_methodref <$> u2 <*> u2
                        11 -> one $ P_interfacemethodref <$> u2 <*> u2
                        12 -> one $ P_nameandtype <$> u2 <*> u2
                        _ -> fail $ path ++ ": constant pool entry #" ++ show mem_index ++ " (D" ++ show i ++ ") has invalid tag: " ++ show tag
                    let
                        new_i = i + length new_entries
                        -- For debugging, uncomment the first definition of 'trace' and comment the second one.
                        -- trace u = Dbg.trace ("#" ++ show mem_index ++ " (D" ++ show i ++ "): " ++ show new_entries) u
                        trace = id
                    trace $ new_i `seq` loop (new_entries ++ result) new_i
            loop [] 0

g_string :: (Integral a) => Get a -> Get ByteString
g_string get_length =
    get_length >>= Se.getByteString . fromIntegral

array_of :: Get a -> Get [a]
array_of body = do
    count <- fromIntegral <$> Se.getWord16be
    M.replicateM count body

-- * Access constant pool

{- $

Every function in this section follows this pattern:

@
cp_get_/something/ :: Class -> Cp_index -> Either String /sometype/
@

-}

cp_get_utf8 :: Class -> Cp_index -> Either String ByteString
cp_get_utf8 c i = do
    e <- cp_get c i
    case e of
        P_utf8 a -> Right a
        _ -> Left "not a UTF8"

cp_get_nameandtype :: Class -> Cp_index -> Either String (Word16, Word16)
cp_get_nameandtype c i = do
    e <- cp_get c i
    case e of
        P_nameandtype a b -> Right (a, b)
        _ -> Left "not a NameAndType"

cp_get_class :: Class -> Cp_index -> Either String Word16
cp_get_class c i = do
    e <- cp_get c i
    case e of
        P_class a -> Right a
        _ -> Left "not a Class"

cp_get_integer :: Class -> Cp_index -> Either String Int32
cp_get_integer c i = do
    e <- cp_get c i
    case e of
        P_integer a -> Right a
        _ -> Left "not an Integer"

cp_get :: Class -> Cp_index -> Either String Constant
cp_get c i = maybe (Left "invalid constant pool index") Right $ Li.at (c_pool c) (fromIntegral i - 1)

{- |
The first constant pool entry has index 1.
-}
type Cp_index = Word16

-- * Parse Code attribute content

data Code
    = Mk_code
    {
        cd_max_stack :: Word16
        , cd_max_local :: Word16
        , cd_code :: ByteString
        , cd_handlers :: [Handler]
        , cd_attributes :: [Attribute]
    }
    deriving (Read, Show, Eq)

{- |
The input is the attribute content.
It does not include the 6-byte attribute header (name and size).
-}
parse_code_attr_content :: ByteString -> Either String Code
parse_code_attr_content =
    Se.runGet grammar
    where
        grammar = do
            max_stack <- u2
            max_local <- u2
            code <- g_string u4 -- XXX can be too big
            handlers <- do
                count <- fromIntegral <$> u2
                M.replicateM count $ Mk_handler <$> u2 <*> u2 <*> u2 <*> u2
            attrs <- g_attributes
            return $ Mk_code max_stack max_local code handlers attrs
        u4 = Se.getWord32be
        u2 = Se.getWord16be
        u1 = Se.getWord8

g_attributes :: Get [Attribute]
g_attributes =
    array_of $ Mk_attribute <$> u2 <*> g_string u4
    where
        u4 = Se.getWord32be
        u2 = Se.getWord16be

data Handler
    = Mk_handler
    {
        h_start_pc :: Word16
        , h_end_pc :: Word16
        , h_handler_pc :: Word16
        , h_catch_type :: Word16
    }
    deriving (Read, Show, Eq)

-- * Parse signature

{- |
The input is a UTF-8 bytestring.
-}
parse_field_type :: ByteString -> Either String Type
parse_field_type =
    either (Left . show) Right . P.parse field_type "" . Bu.toString

{- |
The input is a UTF-8 bytestring.
-}
parse_method_type :: ByteString -> Either String Signature
parse_method_type =
    either (Left . show) Right . P.parse method_type "" . Bu.toString

-- * Low-level IO

slurp :: FilePath -> IO ByteString
slurp = Bs.readFile

-- * Signature parsers

field_type :: Parser Type
field_type =
    Byte <$ P.char 'B'
    <|> Char <$ P.char 'C'
    <|> Double <$ P.char 'D'
    <|> Float <$ P.char 'F'
    <|> Int <$ P.char 'I'
    <|> Long <$ P.char 'J'
    <|> Instance <$> (Bu.fromString <$> (P.char 'L' *> P.manyTill P.anyChar (P.char ';')))
    <|> Short <$ P.char 'S'
    <|> Bool <$ P.char 'Z'
    <|> Array <$ P.char '[' <*> field_type

method_type :: Parser Signature
method_type =
    Mk_signature
    <$> (P.char '(' *> P.many field_type <* P.char ')')
    <*> return_type
    where
        return_type =
            field_type
            <|> Void <$ P.char 'V'

type Parser a = P.Parsec String () a

-- * Types

data Constant
    = P_utf8 ByteString
    | P_integer Int32
    | P_float Float
    | P_long Int64
    | P_double Double
    | P_class Word16
    | P_string Word16
    | P_fieldref Word16 Word16
    | P_methodref Word16 Word16
    | P_interfacemethodref Word16 Word16
    | P_nameandtype Word16 Word16
    | P_unused -- ^ second slot of 8-byte constant
    deriving (Read, Show)

data Attribute
    = Mk_attribute
    {
        a_name :: Word16
        , a_content :: ByteString
    }
    deriving (Read, Show, Eq)

data Field_info
    = Mk_field_info
    {
        fi_access :: Word16
        , fi_name :: Word16
        , fi_descriptor :: Word16
        , fi_attributes :: [Attribute]
    }
    deriving (Read, Show)

data Method_info
    = Mk_method_info
    {
        mi_access :: Word16
        , mi_name :: Word16
        , mi_descriptor :: Word16
        , mi_attributes :: [Attribute]
    }
    deriving (Read, Show)

data Class
    = Mk_class
    {
        c_minor :: Word16
        , c_major :: Word16
        , c_pool :: [Constant]
        , c_access :: Word16
        , c_this :: Word16
        , c_super :: Word16
        , c_ifaces :: [Word16]
        , c_fields :: [Field_info]
        , c_methods :: [Method_info]
        , c_attrs :: [Attribute]
    }
    deriving (Read, Show)
