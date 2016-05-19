{- |
Class file parsing.

The concern of this module is the binary representation of classes,
not the execution.

This is the lowest layer of this JVM implementation.
-}
module Jvm_io
where

import Control.Applicative
    (
        (<|>)
    )

import qualified Control.Monad as M

import Data.Word
    (
        Word8
        , Word16
        , Word32
    )

import qualified Data.ByteString as Bs

import qualified Data.Serialize as Se

import qualified Text.Parsec as P

import qualified Data.ByteString.UTF8 as Bu

-- * Parse class files

{- |
Deserialize the binary representation from disk.
-}
load_class_file :: FilePath -> IO (Either String Class)
load_class_file = fmap parse_class . slurp

parse_class :: Bs.ByteString -> Either String Class
parse_class =
    Se.runGet grammar
    where
        grammar = do
            magic <- u4
            M.unless (magic == 0xcafebabe) $ fail "bad magic"
            minor <- u2
            major <- u2
            pool <- do
                n <- fromIntegral <$> u2
                -- valid constant pool index range is from 1 to (n - 1) inclusive
                M.replicateM (n - 1) $ do
                    tag <- u1
                    case tag of
                        1 -> do
                            count <- fromIntegral <$> u2
                            P_utf8 <$> Se.getByteString count
                        7 -> P_class <$> u2
                        8 -> P_string <$> u2
                        9 -> P_fieldref <$> u2 <*> u2
                        10 -> P_methodref <$> u2 <*> u2
                        11 -> P_interfacemethodref <$> u2 <*> u2
                        12 -> P_nameandtype <$> u2 <*> u2
                        _ -> fail $ "constant pool entry has invalid tag: " ++ show tag
            access <- u2
            this <- u2
            super <- u2
            ifaces <- do
                count <- fromIntegral <$> u2
                M.replicateM count u2
            fields <- do
                count <- fromIntegral <$> u2
                M.replicateM count $
                    Mk_field_info <$> u2 <*> u2 <*> u2 <*> g_attributes
            methods <- do
                count <- fromIntegral <$> u2
                M.replicateM count $
                    Mk_method_info <$> u2 <*> u2 <*> u2 <*> g_attributes
            attrs <- g_attributes
            return $ Mk_class minor major pool access this super ifaces fields methods attrs
        u4 = Se.getWord32be
        u2 = Se.getWord16be
        u1 = Se.getWord8

-- * Access constant pool

{- $

Every function in this section follows this pattern:

@
cp_get_/something/ :: Class -> Cp_index -> Either String /sometype/
@

-}

cp_get_utf8 :: Class -> Cp_index -> Either String Bs.ByteString
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

cp_get :: Class -> Cp_index -> Either String Constant
cp_get c i = maybe (Left "invalid constant pool index") Right $ at (c_pool c) (fromIntegral i - 1)

{- |
The first constant pool entry has index 1.
-}
type Cp_index = Word16

-- * List functions

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at _ i | i < 0 = Nothing
at (x : _) 0 = Just x
at (_ : y) i = at y (i - 1)

-- * Parse Code attribute content

data Code
    = Mk_code
    {
        cd_max_stack :: Word16
        , cd_max_local :: Word16
        , cd_code :: Bs.ByteString
        , cd_handlers :: [Handler]
        , cd_attributes :: [Attribute]
    }
    deriving (Read, Show, Eq)

{- |
The input is the attribute content.
It does not include the 6-byte attribute header (name and size).
-}
parse_code_attr_content :: Bs.ByteString -> Either String Code
parse_code_attr_content =
    Se.runGet grammar
    where
        grammar = do
            max_stack <- u2
            max_local <- u2
            code <- do
                code_length <- fromIntegral <$> u4
                Se.getByteString code_length -- XXX can be too big
            handlers <- do
                count <- fromIntegral <$> u2
                M.replicateM count $ Mk_handler <$> u2 <*> u2 <*> u2 <*> u2
            attrs <- g_attributes
            return $ Mk_code max_stack max_local code handlers attrs
        u4 = Se.getWord32be
        u2 = Se.getWord16be
        u1 = Se.getWord8

g_attributes :: Se.Get [Attribute]
g_attributes = do
    n <- fromIntegral <$> u2
    M.replicateM n $ do
        name <- u2
        count <- fromIntegral <$> u4
        Mk_attribute name <$> Se.getByteString count
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
parse_field_type :: Bs.ByteString -> Either String Type
parse_field_type =
    either (Left . show) Right . P.parse field_type "" . Bu.toString

{- |
The input is a UTF-8 bytestring.
-}
parse_method_type :: Bs.ByteString -> Either String Signature
parse_method_type =
    either (Left . show) Right . P.parse method_type "" . Bu.toString

-- * Low-level IO

slurp :: FilePath -> IO Bs.ByteString
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
    <|> Instance <$> (P.char 'L' *> P.manyTill P.anyChar (P.char ';'))
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
            Just <$> field_type
            <|> Nothing <$ P.char 'V'

data Type
    = Byte
    | Char
    | Double
    | Float
    | Int
    | Long
    | Instance String -- ^ class name
    | Short
    | Bool
    | Array Type
    deriving (Read, Show, Eq)

data Signature
    = Mk_signature
    {
        s_arg_types :: [Type]
        , s_return_type :: Return_type
    }
    deriving (Read, Show, Eq)

type Return_type = Maybe Type

type Parser a = P.Parsec String () a

-- * Types

data Constant
    = P_utf8 Bs.ByteString
    | P_class Word16
    | P_string Word16
    | P_fieldref Word16 Word16
    | P_methodref Word16 Word16
    | P_interfacemethodref Word16 Word16
    | P_nameandtype Word16 Word16
    deriving (Read, Show)

data Attribute
    = Mk_attribute
    {
        a_name :: Word16
        , a_content :: Bs.ByteString
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
