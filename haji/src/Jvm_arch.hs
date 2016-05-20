module Jvm_arch
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
        , Int64
    )
import Data.Word
    (
        Word8
        , Word16
        , Word32
    )

import qualified Control.Monad as M

import qualified Data.ByteString as Bs

import qualified Data.ByteString.UTF8 as Bu

import Jvm_io (Code(..))
import Jvm_value (Value(..))
import Jvm_type
    (
        Signature(..)
        , Type(..)
    )

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

class (Monad m) => Monad_state m where
    get :: m State
    put :: State -> m ()
    stop :: Status -> m a

    gets :: (State -> a) -> m a
    gets f = f <$> get

    modify :: (State -> State) -> m ()
    modify f = get >>= put . f

instance Monad_state S where
    get = Mk_s $ \ s -> (s, Just s)
    put s = Mk_s $ \ _ -> (s, Just ())
    stop reason = Mk_s (\ s -> (s { s_status = reason }, Nothing))

-- * J Monad

newtype J a = Mk_j { un_j :: State -> IO (State, Maybe a) }

instance Functor J where
    fmap f m = Mk_j $ \ s ->
        fmap
            (\ (s, e) -> (s, fmap f e))
            (un_j m s)

instance Applicative J where
    pure = return
    (<*>) = M.ap

instance Monad J where
    return a = Mk_j $ \ s -> return (s, Just a)
    (>>=) m k = Mk_j $ \ s_0 -> do
        case is_ready s_0 of
            False ->
                return (s_0, Nothing)
            True -> do
                (s_1, ma) <- un_j m s_0
                case is_ready s_1 of
                    False -> return (s_1, Nothing)
                    True -> case ma of
                        Nothing -> return (s_1, Nothing)
                        Just a -> un_j (k a) s_1

instance Monad_state J where
    get = lift get
    put = lift . put
    stop = lift . stop

lift :: S a -> J a
lift x = Mk_j $ return . un_s x

exec :: S a -> State -> State
exec comp init_state = fst $ un_s comp init_state

exec_io :: J a -> State -> IO State
exec_io comp init_state = fst <$> un_j comp init_state

-- * State and Frame

-- | Create a state.
s_new :: Frame -> State
s_new frame = Mk_state Ready frame [] [f_class frame]

-- | Create a frame.
f_new :: Class -> Method -> Frame
f_new cls met = Mk_frame cls met 0 [] []

class Is_ready a where is_ready :: a -> Bool

instance Is_ready State where
    is_ready = is_ready . s_status

instance Is_ready Status where
    is_ready Ready = True
    is_ready _ = False

-- * Internals

data State
    = Mk_state
    {
        s_status :: Status
        , s_frame :: Frame -- ^ current frame (topmost frame in the frame stack)
        , s_frames :: [Frame] -- ^ the other frames in the frame stack
        , s_classes :: [Class] -- ^ loaded classes
    }

data Status
    = Ready
    | End_of_program
    | Operand_stack_underflow
    | Invalid_constant_pool_index
    | Invalid_local_index
    | Method_lacks_Code
    | Method_body_missing
    | Method_body_not_fetchable
    | Invalid_pc Pc
    | Decode_error Word8
    | Unknown_instruction Instruction
    | Need_io -- ^ usually requested by a native method
    | Expecting_type Type -- ^ expected-type actual-value ; value has wrong type
    | Expecting_fieldref
    | Expecting_methodref
    | Class_not_found Class_name
    | Field_not_found Class_name Fieldref
    | Field_not_initialized Class_name Fieldref
    | Method_not_found Class_name Method_name Signature
    | Invalid_tableswitch String
    | Invalid_lookupswitch String
    | Invalid_reserved Int Word8
    | Not_implemented String
    deriving (Read, Show, Eq)

{- |
Every method call creates one frame.

Every method return destroys one frame.
-}
data Frame
    = Mk_frame
    {
        f_class :: Class
        , f_method :: Method
        , f_pc :: Pc
        , f_stack :: [Value] -- ^ operand stack
        , f_local :: [Value] -- ^ local variable array
    }

{- |
Program counter type.

'Pc' must be unsigned,
'Pc_offset' must be signed,
and they must have the same width.
-}
type Pc = Word32

type Pc_offset = Int32

-- * Instructions

{- |
https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-7.html
-}
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
    | Fconst_0 -- ^ 11
    | Fconst_1 -- ^ 12
    | Fconst_2 -- ^ 13
    | Dconst_0 -- ^ 14
    | Dconst_1 -- ^ 15
    | Bipush Int8 -- ^ 16
    | Sipush Int16 -- ^ 17
    | Ldc Word8 -- ^ 18
    | Ldc_w Word16 -- ^ 19
    | Ldc2_w Word16 -- ^ 20
    | Iload Word8 -- ^ 21
    | Lload Word8 -- ^ 22
    | Fload Word8 -- ^ 23
    | Dload Word8 -- ^ 24
    | Aload Word8 -- ^ 25
    | Iload_0 -- ^ 26
    | Iload_1 -- ^ 27
    | Iload_2 -- ^ 28
    | Iload_3 -- ^ 29
    | Lload_0 -- ^ 30
    | Lload_1 -- ^ 31
    | Lload_2 -- ^ 32
    | Lload_3 -- ^ 33
    | Fload_0 -- ^ 34
    | Fload_1 -- ^ 35
    | Fload_2 -- ^ 36
    | Fload_3 -- ^ 37
    | Dload_0 -- ^ 38
    | Dload_1 -- ^ 39
    | Dload_2 -- ^ 40
    | Dload_3 -- ^ 41
    | Aload_0 -- ^ 42
    | Aload_1 -- ^ 43
    | Aload_2 -- ^ 44
    | Aload_3 -- ^ 45
    | Iaload -- ^ 46
    | Laload -- ^ 47
    | Faload -- ^ 48
    | Daload -- ^ 49
    | Aaload -- ^ 50
    | Baload -- ^ 51
    | Caload -- ^ 52
    | Saload -- ^ 53
    | Istore Word8 -- ^ 54
    | Lstore Word8 -- ^ 55
    | Fstore Word8 -- ^ 56
    | Dstore Word8 -- ^ 57
    | Astore Word8 -- ^ 58
    | Istore_0 -- ^ 59
    | Istore_1 -- ^ 60
    | Istore_2 -- ^ 61
    | Istore_3 -- ^ 62
    | Lstore_0 -- ^ 63
    | Lstore_1 -- ^ 64
    | Lstore_2 -- ^ 65
    | Lstore_3 -- ^ 66
    | Fstore_0 -- ^ 67
    | Fstore_1 -- ^ 68
    | Fstore_2 -- ^ 69
    | Fstore_3 -- ^ 70
    | Dstore_0 -- ^ 71
    | Dstore_1 -- ^ 72
    | Dstore_2 -- ^ 73
    | Dstore_3 -- ^ 74
    | Astore_0 -- ^ 75
    | Astore_1 -- ^ 76
    | Astore_2 -- ^ 77
    | Astore_3 -- ^ 78
    | Iastore -- ^ 79
    | Lastore -- ^ 80
    | Fastore -- ^ 81
    | Dastore -- ^ 82
    | Aastore -- ^ 83
    | Bastore -- ^ 84
    | Castore -- ^ 85
    | Sastore -- ^ 86
    | Pop -- ^ 87
    | Pop2 -- ^ 88
    | Dup -- ^ 89
    | Dup_x1 -- ^ 90
    | Dup_x2 -- ^ 91
    | Dup2 -- ^ 92
    | Dup2_x1 -- ^ 93
    | Dup2_x2 -- ^ 94
    | Swap -- ^ 95
    | Iadd -- ^ 96
    | Ladd -- ^ 97
    | Fadd -- ^ 98
    | Dadd -- ^ 99
    | Isub -- ^ 100
    | Lsub -- ^ 101
    | Fsub -- ^ 102
    | Dsub -- ^ 103
    | Imul -- ^ 104
    | Lmul -- ^ 105
    | Fmul -- ^ 106
    | Dmul -- ^ 107
    | Idiv -- ^ 108
    | Ldiv -- ^ 109
    | Fdiv -- ^ 110
    | Ddiv -- ^ 111
    | Irem -- ^ 112
    | Lrem -- ^ 113
    | Frem -- ^ 114
    | Drem -- ^ 115
    | Ineg -- ^ 116
    | Lneg -- ^ 117
    | Fneg -- ^ 118
    | Dneg -- ^ 119
    | Ishl -- ^ 120
    | Lshl -- ^ 121
    | Ishr -- ^ 122
    | Lshr -- ^ 123
    | Iushr -- ^ 124
    | Lushr -- ^ 125
    | Iand -- ^ 126
    | Land -- ^ 127
    | Ior -- ^ 128
    | Lor -- ^ 129
    | Ixor -- ^ 130
    | Lxor -- ^ 131
    | Iinc Word8 Int8 -- ^ 132
    | I2l -- ^ 133
    | I2f -- ^ 134
    | I2d -- ^ 135
    | L2i -- ^ 136
    | L2f -- ^ 137
    | L2d -- ^ 138
    | F2i -- ^ 139
    | F2l -- ^ 140
    | F2d -- ^ 141
    | D2i -- ^ 142
    | D2l -- ^ 143
    | D2f -- ^ 144
    | I2b -- ^ 145
    | I2c -- ^ 146
    | I2s -- ^ 147
    | Lcmp -- ^ 148
    | Fcmpl -- ^ 149
    | Fcmpg -- ^ 150
    | Dcmpl -- ^ 151
    | Dcmpg -- ^ 152
    | Ifeq Int16 -- ^ 153
    | Ifne Int16 -- ^ 154
    | Iflt Int16 -- ^ 155
    | Ifge Int16 -- ^ 156
    | Ifgt Int16 -- ^ 157
    | Ifle Int16 -- ^ 158
    | If_icmpeq Int16 -- ^ 159
    | If_icmpne Int16 -- ^ 160
    | If_icmplt Int16 -- ^ 161
    | If_icmpge Int16 -- ^ 162
    | If_icmpgt Int16 -- ^ 163
    | If_icmple Int16 -- ^ 164
    | If_acmpeq Int16 -- ^ 165
    | If_acmpne Int16 -- ^ 166
    | Goto Int16 -- ^ 167
    | Jsr Int16 -- ^ 168
    | Ret Word8 -- ^ 169
    | Tableswitch Int32 Int32 Int32 [Int32] -- ^ 170 def low high offsets
    | Ireturn -- ^ 172
    | Lreturn -- ^ 173
    | Freturn -- ^ 174
    | Dreturn -- ^ 175
    | Areturn -- ^ 176
    | Return -- ^ 177
    | Getstatic Word16 -- ^ 178
    | Putstatic Word16 -- ^ 179
    | Getfield Word16 -- ^ 180
    | Putfield Word16 -- ^ 181
    | Invokevirtual Word16 -- ^ 182
    | Invokespecial Word16 -- ^ 183
    | Invokestatic Word16 -- ^ 184
    | Invokeinterface Word16 Word8 -- ^ 185
    | Invokedynamic Word16 -- ^ 186
    | New Word16 -- ^ 187
    | Anewarray Word16 -- ^ 189
    | Arraylength -- ^ 190
    | Athrow -- ^ 191
    | Checkcast Word16 -- ^ 192
    | Instanceof Word16 -- ^ 193
    | Monitorenter -- ^ 194
    | Monitorexit -- ^ 195
    | Ifnull Int16 -- ^ 198
    | Ifnonnull Int16 -- ^ 199
    | Goto_w Int32 -- ^ 200
    | Jsr_w Int32 -- ^ 201
    | Breakpoint -- ^ 202
    | Impdep1 -- ^ 254
    | Impdep2 -- ^ 255
    deriving (Read, Show, Eq)

-- * Types suitable for execution

data Class
    = Mk_class
    {
        c_name :: Bs.ByteString
        , c_fields :: [Field]
        , c_methods :: [Method]
        , c_pool :: [Constant]
        , c_static :: [(Fieldref, Value)] -- ^ static field values
    }

{- |
An inhabitant of this type is an entry in a constant pool that has been resolved.
-}
data Constant
    = C_class Bs.ByteString
    | C_string Bs.ByteString
    | C_fieldref Class_name Field_name Type
    | C_methodref Class_name Method_name Signature
    | C_resolved
    deriving (Read, Show, Eq)

type Class_name = Bs.ByteString
type Field_name = Bs.ByteString
type Method_name = Bs.ByteString

data Attribute
    = Mk_attribute
    {
        a_name :: Bs.ByteString
        , a_content :: Bs.ByteString
    }
    deriving (Read, Show)

data Field
    = Mk_field
    {
        f_access :: Word16
        , f_name :: Field_name
        , f_type :: Type
    }
    deriving (Read, Show, Eq)

data Method
    = Mk_method
    {
        m_access :: Word16
        , m_name :: Method_name
        , m_signature :: Signature
        , m_body :: Body
    }

data Body
    = Missing -- ^ no body
    | Bytecode Code -- ^ JVM bytecode
    | Native (S Value) -- ^ native without 'IO'
    | Native_io (J Value) -- ^ native with 'IO'

data Fieldref
    = Mk_fieldref Field_name Type
    deriving (Read, Show, Eq)
