{- |
Virtual machine state
-}

module Jvm_state
where

import Data.Bits
    (
        (.&.)
        , (.|.)
        , unsafeShiftL
    )
import Data.Int
    (
        Int8
        , Int16
        , Int32
    )
import Data.Word
    (
        Word8
        , Word16
        , Word32
    )

import qualified Control.Monad as M

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Unsafe as Bsu

import qualified Data.ByteString.UTF8 as Bu

import qualified Jvm_io as Z

-- * Dereferencing constant pool

load_class_file :: FilePath -> IO (Either String Class)
load_class_file = fmap (>>= resolve_class) . Z.load_class_file

resolve_class :: Z.Class -> Either String Class
resolve_class c = do
    Mk_class
    <$> (Z.cp_get_class c (Z.c_this c) >>= Z.cp_get_utf8 c)
    <*> methods
    where
        methods =
            M.forM (Z.c_methods c) $ \ m -> do
                name <- Z.cp_get_utf8 c (Z.mi_name m)
                type_ <- Z.cp_get_utf8 c (Z.mi_descriptor m) >>= Z.parse_method_type
                attrs <- mapM (resolve_attribute c) (Z.mi_attributes m)
                let contents = [ c | Mk_attribute name c <- attrs
                        , name == Bu.fromString "Code" ]
                content <- case contents of
                    [x] -> Right x
                    [] -> Left "method has no Code attribute"
                    _ -> Left "method has too many Code attributes"
                code <- Z.parse_code_attr_content content
                return $ Mk_method (Z.mi_access m) name type_ code

resolve_attribute :: Z.Class -> Z.Attribute -> Either String Attribute
resolve_attribute c a =
    Mk_attribute
        <$> Z.cp_get_utf8 c (Z.a_name a)
        <*> pure (Z.a_content a)

-- * Testing access flags; Method list operations

name_is :: String -> Method -> Bool
name_is name m = m_name m == Bu.fromString name

is_main :: Method -> Bool
is_main m =
    is_static m && name_is "main" m
    && Z.s_arg_types (m_signature m) == [Z.Array (Z.Instance "java/lang/String")]

is_static :: Method -> Bool
is_static m = m_access m .&. 0x0008 /= 0

is_public :: Method -> Bool
is_public m = m_access m .&. 0x0001 /= 0

-- * Types tailored for execution

data Attribute
    = Mk_attribute
    {
        a_name :: Bs.ByteString
        , a_content :: Bs.ByteString
    }
    deriving (Read, Show)

data Class
    = Mk_class
    {
        c_name :: Bs.ByteString
        , c_methods :: [Method]
    }
    deriving (Read, Show)

data Method
    = Mk_method
    {
        m_access :: Word16
        , m_name :: Bs.ByteString
        , m_signature :: Z.Signature
        , m_code :: Z.Code
    }
    deriving (Read, Show)

-- * State

data State
    = Mk_state
    {
        s_running :: Bool
        , s_stop_reason :: Reason
        , s_frame :: Frame -- ^ current frame (topmost frame in the frame stack)
        , s_frames :: [Frame] -- ^ rest of frames in the frame stack
    }
    deriving (Show)

{- |
When an instruction needs 'IO' but the execution happens in 'S',
the machine throws a 'Need_io' and rewinds the program counter
to point to that faulting instruction so that
resuming the machine in J retries that instruction.
-}
data Reason
    = End_of_program
    | Operand_stack_underflow
    | Invalid_constant_pool_index
    | Invalid_local_index
    | Method_lacks_Code
    | Invalid_pc Pc
    | Decode_error Word8
    | Unknown_instruction String
    | Need_io
    deriving (Show)

s_new :: Frame -> State
s_new frame = Mk_state True End_of_program frame []

s_code :: State -> Bs.ByteString
s_code = Z.cd_code . m_code . f_method . s_frame

-- * Frame

f_new :: Class -> Method -> Frame
f_new cls met = Mk_frame cls met 0 [] []

-- | Program counter.
type Pc = Word32

data Frame
    = Mk_frame
    {
        f_class :: Class
        , f_method :: Method
        , f_pc :: Pc
        , f_stack :: [Value] -- ^ operand stack
        , f_local :: [Value] -- ^ local variable array
    }
    deriving (Show)

-- * Value

-- | A constant pool value, or an element in the stack.
data Value
    = Integer Int32
    deriving (Read, Show)

-- * S Monad, like J but without IO

newtype S a = Mk_s { un_s :: State -> (State, Maybe a) }

instance Functor S where
    fmap f w = Mk_s $ \ s0 ->
        let
            (s1, m) = un_s w s0
        in
            (s1, f <$> m)

instance Applicative S where
    pure = return
    (<*>) = M.ap

instance Monad S where
    return x = Mk_s $ \ s -> (s, Just x)
    (>>=) w k = Mk_s $ \ s0 ->
        let
            (s1, ma) = un_s w s0
            (s2, mb) = maybe (s1, Nothing) (\ a -> un_s (k a) s1) ma
        in
            (s2, mb)

s_pc :: State -> Pc
s_pc = f_pc . s_frame

get_pc :: S Pc
get_pc = gets s_pc

gets :: (State -> a) -> S a
gets f = f <$> get

get :: S State
get = Mk_s $ \ s -> (s, Just s)

put :: State -> S ()
put s = Mk_s $ \ _ -> (s, Just ())

modify :: (State -> State) -> S ()
modify f = Mk_s $ \ s -> (f s, Just ())

-- * Disassembly

disassemble :: Class -> Method -> (State, [(Pc, Instruction)])
disassemble c m = disassemble_ (s_new (f_new c m))

disassemble_ :: State -> (State, [(Pc, Instruction)])
disassemble_ s0 =
    case un_s decode s0 of
        (s1, Nothing) -> (s1, [])
        (s1, Just i) ->
            let
                (s2, j) = disassemble_ s1
            in
                (s2, (f_pc (s_frame s0), i) : j)

-- * Non-IO state manipulators

fetch :: S Word8
fetch = Mk_s fetch_raw
    where
        fetch_raw s =
            case () of
                _ | ipc < Bs.length code ->
                    (s { s_frame = frame { f_pc = pc + 1 } }, Just $ Bsu.unsafeIndex code ipc)
                _ ->
                    (stop_raw (Invalid_pc pc) s, Nothing)
            where
                frame = s_frame s
                code = s_code s
                pc = f_pc frame
                ipc = fromIntegral pc :: Int -- FIXME what if pc doesn't fit in an Int?

{- |
Fetch and decode the next instruction, and increment pc accordingly.
-}
decode :: S Instruction
decode = do
    op <- fetch
    case op of
        0 -> return Nop
        1 -> return Aconst_null
        2 -> return Iconst_m1
        3 -> return Iconst_0
        4 -> return Iconst_1
        5 -> return Iconst_2
        6 -> return Iconst_3
        7 -> return Iconst_4
        8 -> return Iconst_5
        9 -> return Lconst_0
        10 -> return Lconst_1
        13 -> return Fconst_2
        14 -> return Dconst_0
        15 -> return Dconst_1
        18 -> Ldc <$> u1
        21 -> Iload <$> u1
        22 -> Lload <$> u1
        26 -> return Iload_0
        27 -> return Iload_1
        42 -> return Aload_0
        60 -> return Istore_1
        132 -> Iinc <$> u1 <*> s1
        162 -> If_icmpge <$> s2
        167 -> Goto <$> s2
        170 -> do
            align4
            def <- s4
            low <- s4
            high <- s4
            let count = fromIntegral $ high - low + 1 -- XXX
            -- assert $ low <= high
            Tableswitch def low high <$> M.replicateM count s4
        172 -> return Ireturn
        177 -> return Return
        178 -> Getstatic <$> u2
        182 -> Invokevirtual <$> u2
        183 -> Invokespecial <$> u2
        184 -> Invokestatic <$> u2
        _ -> stop $ Decode_error op
    where
        -- XXX this aligns with respect to the beginning of code[]
        align4 = do
            p <- get_pc
            M.replicateM_ (fromIntegral $ 4 - mod p 4) u1
        u1 = fetch
        s1 = fromIntegral <$> u1
        u2 :: S Word16
        u2 = do
            a <- u1
            b <- u1
            return (unsafeShiftL (fromIntegral a) 8 .|. fromIntegral b)
        u4 :: S Word32
        u4 = do
            a <- u1
            b <- u1
            c <- u1
            d <- u1
            return $
                unsafeShiftL (fromIntegral a) 24
                .|. unsafeShiftL (fromIntegral b) 16
                .|. unsafeShiftL (fromIntegral c) 8
                .|. fromIntegral d
        s4 :: S Int32
        s4 = fromIntegral <$> u4
        -- XXX where is the documentation for fromIntegral?
        s2 :: S Int16
        s2 = fromIntegral <$> u2
-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-7.html

data Instruction
    = Nop -- ^ 0
    | Aconst_null -- ^ 1
    | Iconst_m1 -- ^ 2
    | Iconst_0 -- ^ 3
    | Iconst_1 -- ^ 4
    | Iconst_2 -- ^ 5
    | Iconst_3 -- ^ 6
    | Iconst_4 -- ^ 7
    | Iconst_5 -- ^ 8
    | Lconst_0 -- ^ 9
    | Lconst_1 -- ^ 10
    | Fconst_2 -- ^ 13
    | Dconst_0 -- ^ 14
    | Dconst_1 -- ^ 15
    | Ldc Word8 -- ^ 18
    | Iload Word8 -- ^ 21
    | Lload Word8 -- ^ 22
    | Iload_0 -- ^ 26
    | Iload_1 -- ^ 27
    | Aload_0 -- ^ 42
    | Istore_1 -- ^ 60
    | Iinc Word8 Int8 -- ^ 132
    | If_icmpge Int16 -- ^ 162
    | Goto Int16 -- ^ 167
    | Tableswitch Int32 Int32 Int32 [Int32] -- ^ 170 def low high offsets
    | Ireturn -- ^ 172
    | Return -- ^ 177
    | Getstatic Word16 -- ^ 178
    | Invokevirtual Word16 -- ^ 182
    | Invokespecial Word16 -- ^ 183
    | Invokestatic Word16 -- ^ 184
    | Istore Word16
    deriving (Read, Show)

stop :: Reason -> S a
stop r = modify (stop_raw r) >> Mk_s (\ s -> (s, Nothing))

leave :: State -> State
leave s =
    case s_frames s of
        f : g -> s { s_frame = f, s_frames = g }
        [] -> stop_raw End_of_program s

stop_raw :: Reason -> State -> State
stop_raw reason s = s { s_running = False, s_stop_reason = reason }

push :: Value -> State -> State
push v s =
    s { s_frame = frame { f_stack = v : stack } }
    where
        frame = s_frame s
        stack = f_stack frame

pop :: State -> (State, Maybe Value)
pop s =
    case stack of
        [] -> (stop_raw Operand_stack_underflow s, Nothing)
        x : y -> (s { s_frame = frame { f_stack = y } }, Just x)
    where
        frame = s_frame s
        stack = f_stack frame
