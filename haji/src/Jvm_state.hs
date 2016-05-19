{- |
This concern of this module is the execution of codes in class files.
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
        , Int64
    )
import Data.Word
    (
        Word8
        , Word16
        , Word32
    )

import qualified Control.Monad as M
import qualified Data.List as L

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Unsafe as Bsu

import qualified Data.ByteString.UTF8 as Bu

import qualified Jvm_io as Z

-- * Loading for execution

{- |
This uses 'Class' from this module, not the 'Z.Class' from "Jvm_io".
-}
load_class_file :: FilePath -> IO (Either String Class)
load_class_file = fmap (>>= resolve_class) . Z.load_class_file

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

-- * Preparing for execution

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
        , c_fields :: [Field]
        , c_methods :: [Method]
        , c_pool :: [Constant]
        , c_static :: [(Fieldref, Value)] -- ^ static field values
    }
    deriving (Read, Show, Eq)

{- |
An inhabitant of this type is an entry in a constant pool that has been resolved.
-}
data Constant
    = C_class Bs.ByteString
    | C_string Bs.ByteString
    | C_fieldref Class_name Field_name Z.Type
    | C_methodref Class_name Method_name Z.Signature
    | C_resolved
    deriving (Read, Show, Eq)

type Class_name = Bs.ByteString
type Field_name = Bs.ByteString
type Method_name = Bs.ByteString

data Field
    = Mk_field
    {
        f_access :: Word16
        , f_name :: Bs.ByteString
        , f_type :: Z.Type
    }
    deriving (Read, Show, Eq)

data Method
    = Mk_method
    {
        m_access :: Word16
        , m_name :: Bs.ByteString
        , m_signature :: Z.Signature
        , m_code :: Maybe Z.Code
    }
    deriving (Read, Show, Eq)

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

-- * Value

-- | An inhabitant of this type is an element in the operand stack.
data Value
    = Padding -- ^ This should follow a Long and a Double, just to make the indexes right
    | Null -- ^ can we use Null for Padding?
    | Byte Int8 -- FIXME Word8 or Int8?
    | Short Int16
    | Integer Int32
    | Long Int64
    | Float Float
    | Double Double
    | Instance Class (Maybe Object)
    deriving (Read, Show, Eq)

data Object
    = Mk_object
    {
        o_fields :: [(Bs.ByteString, Value)]
    }
    deriving (Read, Show, Eq)

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

gets :: (State -> a) -> S a
gets f = f <$> get

get :: S State
get = Mk_s $ \ s -> (s, Just s)

put :: State -> S ()
put s = Mk_s $ \ _ -> (s, Just ())

modify :: (State -> State) -> S ()
modify f = Mk_s $ \ s -> (f s, Just ())

-- * Bytecode execution

-- ** Decoding instructions

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
        11 -> return Fconst_0
        12 -> return Fconst_1
        13 -> return Fconst_2
        14 -> return Dconst_0
        15 -> return Dconst_1
        16 -> Bipush <$> s1
        17 -> Sipush <$> s2
        18 -> Ldc <$> u1
        19 -> Ldc_w <$> u2
        20 -> Ldc2_w <$> u2
        21 -> Iload <$> u1
        22 -> Lload <$> u1
        23 -> Fload <$> u1
        24 -> Dload <$> u1
        25 -> Aload <$> u1
        26 -> return Iload_0
        27 -> return Iload_1
        28 -> return Iload_2
        29 -> return Iload_3
        30 -> return Lload_0
        31 -> return Lload_1
        32 -> return Lload_2
        33 -> return Lload_3
        34 -> return Fload_0
        35 -> return Fload_1
        36 -> return Fload_2
        37 -> return Fload_3
        38 -> return Dload_0
        39 -> return Dload_1
        40 -> return Dload_2
        41 -> return Dload_3
        42 -> return Aload_0
        43 -> return Aload_1
        44 -> return Aload_2
        45 -> return Aload_3
        46 -> return Iaload
        47 -> return Laload
        48 -> return Faload
        49 -> return Daload
        50 -> return Aaload
        51 -> return Baload
        52 -> return Caload
        53 -> return Saload
        54 -> Istore <$> u1
        55 -> Lstore <$> u1
        56 -> Fstore <$> u1
        57 -> Dstore <$> u1
        58 -> Astore <$> u1
        59 -> return Istore_0
        60 -> return Istore_1
        61 -> return Istore_2
        62 -> return Istore_3
        63 -> return Lstore_0
        64 -> return Lstore_1
        65 -> return Lstore_2
        66 -> return Lstore_3
        67 -> return Fstore_0
        68 -> return Fstore_1
        69 -> return Fstore_2
        70 -> return Fstore_3
        71 -> return Dstore_0
        72 -> return Dstore_1
        73 -> return Dstore_2
        74 -> return Dstore_3
        75 -> return Astore_0
        76 -> return Astore_1
        77 -> return Astore_2
        78 -> return Astore_3
        79 -> return Iastore
        80 -> return Lastore
        81 -> return Fastore
        82 -> return Dastore
        83 -> return Aastore
        84 -> return Bastore
        85 -> return Castore
        86 -> return Sastore
        87 -> return Pop
        88 -> return Pop2
        89 -> return Dup
        90 -> return Dup_x1
        91 -> return Dup_x2
        92 -> return Dup2
        93 -> return Dup2_x1
        94 -> return Dup2_x2
        95 -> return Swap
        96 -> return Iadd
        97 -> return Ladd
        98 -> return Fadd
        99 -> return Dadd
        100 -> return Isub
        101 -> return Lsub
        102 -> return Fsub
        103 -> return Dsub
        104 -> return Imul
        105 -> return Lmul
        106 -> return Fmul
        107 -> return Dmul
        108 -> return Idiv
        109 -> return Ldiv
        110 -> return Fdiv
        111 -> return Ddiv
        112 -> return Irem
        113 -> return Lrem
        114 -> return Frem
        115 -> return Drem
        116 -> return Ineg
        117 -> return Lneg
        118 -> return Fneg
        119 -> return Dneg
        120 -> return Ishl
        121 -> return Lshl
        122 -> return Ishr
        123 -> return Lshr
        124 -> return Iushr
        125 -> return Lushr
        126 -> return Iand
        127 -> return Land
        128 -> return Ior
        129 -> return Lor
        130 -> return Ixor
        131 -> return Lxor
        132 -> Iinc <$> u1 <*> s1
        133 -> return I2l
        134 -> return I2f
        135 -> return I2d
        136 -> return L2i
        137 -> return L2f
        138 -> return L2d
        139 -> return F2i
        140 -> return F2l
        141 -> return F2d
        142 -> return D2i
        143 -> return D2l
        144 -> return D2f
        145 -> return I2b
        146 -> return I2c
        147 -> return I2s
        148 -> return Lcmp
        149 -> return Fcmpl
        150 -> return Fcmpg
        151 -> return Dcmpl
        152 -> return Dcmpg
        153 -> Ifeq <$> s2
        154 -> Ifne <$> s2
        155 -> Iflt <$> s2
        156 -> Ifge <$> s2
        157 -> Ifgt <$> s2
        158 -> Ifle <$> s2
        159 -> If_icmpeq <$> s2
        160 -> If_icmpne <$> s2
        161 -> If_icmplt <$> s2
        162 -> If_icmpge <$> s2
        163 -> If_icmpgt <$> s2
        164 -> If_icmple <$> s2
        165 -> If_acmpeq <$> s2
        166 -> If_acmpne <$> s2
        167 -> Goto <$> s2
        168 -> Jsr <$> s2
        169 -> Ret <$> u1
        170 -> do
            align4
            def <- s4
            low <- s4
            high <- s4
            let count = fromIntegral (high - low + 1) -- XXX
            M.unless (low <= high) $ stop (Invalid_tableswitch "high > low")
            Tableswitch def low high <$> M.replicateM count s4
        172 -> return Ireturn
        173 -> return Lreturn
        174 -> return Freturn
        175 -> return Dreturn
        176 -> return Areturn
        177 -> return Return
        178 -> Getstatic <$> u2
        179 -> Putstatic <$> u2
        180 -> Getfield <$> u2
        181 -> Putfield <$> u2
        182 -> Invokevirtual <$> u2
        183 -> Invokespecial <$> u2
        184 -> Invokestatic <$> u2
        185 -> Invokeinterface <$> u2 <*> u1 <* mbz 4
        186 -> Invokedynamic <$> u2 <* mbz 3 <* mbz 4
        187 -> New <$> u2
        189 -> Anewarray <$> u2
        190 -> return Arraylength
        191 -> return Athrow
        192 -> Checkcast <$> u2
        193 -> Instanceof <$> u2
        194 -> return Monitorenter
        195 -> return Monitorexit
        198 -> Ifnull <$> s2
        199 -> Ifnonnull <$> s2
        200 -> Goto_w <$> s4
        201 -> Jsr_w <$> s4
        202 -> return Breakpoint
        254 -> return Impdep1
        255 -> return Impdep2
        _ -> stop (Decode_error op)
    where
        -- must be zero
        mbz n = u1 >>= \ a -> if a == 0 then return () else stop (Invalid_reserved n a)
        -- XXX this aligns with respect to the beginning of code[]
        align4 = do
            p <- get_pc
            M.replicateM_ (fromIntegral $ 4 - mod p 4) u1
        get_pc :: S Pc
        get_pc = gets (f_pc . s_frame)
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
        fetch :: S Word8
        fetch = Mk_s fetch_raw
            where
                s_code :: State -> Maybe Bs.ByteString
                s_code = fmap Z.cd_code . m_code . f_method . s_frame
                fetch_raw s =
                    case s_code s of
                        Nothing -> (stop_raw Method_lacks_Code s, Nothing)
                        Just code ->
                            case () of
                                _ | ipc < Bs.length code ->
                                    (s { s_frame = frame { f_pc = pc + 1 } }, Just $ Bsu.unsafeIndex code ipc)
                                _ ->
                                    (stop_raw (Invalid_pc pc) s, Nothing)
                    where
                        frame = s_frame s
                        pc = f_pc frame
                        ipc = fromIntegral pc :: Int -- FIXME what if pc doesn't fit in an Int?
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

-- ** Executing an instruction

{- |
Decode the next instruction and execute it.

The program counter is incremented accordingly.
-}
step :: S ()
step = decode >>= execute

execute :: Instruction -> S ()
execute instruction =
    -- XXX should use vector (or Map Int) instead of list
    case instruction of
        Nop -> return ()
        -- Lload i -> get_local i >>= push
        Getstatic c -> do
            (cname, fname, ftype) <- cp_get c >>= cp_want_fieldref
            target_class <- load_class cname
            -- target_field <- get_field target_class fname ftype
            val <- get_static_field cname (Mk_fieldref fname ftype)
            push val
        Iconst_m1 -> iconst (negate 1)
        Iconst_0 -> iconst 0
        Iconst_1 -> iconst 1
        Iconst_2 -> iconst 2
        Iconst_3 -> iconst 3
        Iconst_4 -> iconst 4
        Iconst_5 -> iconst 5
        Bipush b -> push (Integer $ fromIntegral b)
        -- TODO check type
        Iload_0 -> load 0 >>= push
        Iload_1 -> load 1 >>= push
        Iload_2 -> load 2 >>= push
        Iload_3 -> load 3 >>= push
        Goto ofs -> add_pc (ofs - 3)
        Istore_0 -> pop_integer >>= store 0 . Integer
        Istore_1 -> pop_integer >>= store 1 . Integer
        Istore_2 -> pop_integer >>= store 2 . Integer
        Istore_3 -> pop_integer >>= store 3 . Integer
        Iadd -> ((+) <$> pop_integer <*> pop_integer) >>= push . Integer
        Ladd -> do
            b <- pop >>= want_long
            a <- pop >>= want_long
            push $ Long $ a + b
        Fadd -> do
            b <- pop >>= want_float
            a <- pop >>= want_float
            push $ Float $ a + b
        Dadd -> do
            b <- pop >>= want_double
            a <- pop >>= want_double
            push $ Double $ a + b
        Isub -> (subtract <$> pop_integer <*> pop_integer) >>= push . Integer
        Ifeq ofs -> pop_integer >>= \ i -> add_pc_if (i == 0) (ofs - 3)
        Ifne ofs -> pop_integer >>= \ i -> add_pc_if (i /= 0) (ofs - 3)
        Iflt ofs -> pop_integer >>= \ i -> add_pc_if (i < 0) (ofs - 3)
        Ifge ofs -> pop_integer >>= \ i -> add_pc_if (i >= 0) (ofs - 3)
        Ifgt ofs -> pop_integer >>= \ i -> add_pc_if (i > 0) (ofs - 3)
        Ifle ofs -> pop_integer >>= \ i -> add_pc_if (i <= 0) (ofs - 3)
        If_icmpge ofs -> do
            -- XXX
            x <- pop >>= want_integer
            y <- pop >>= want_integer
            let success = x >= y
            -- The offset is calculated from the beginning of the If instruction.
            M.when success $ add_pc (ofs - 3)
        Iinc i_ c ->
            let
                i = fromIntegral i_
            in
                load i
                >>= want_integer
                >>= store i . Integer . (fromIntegral c +)
        -- Iload im -> get_local im >>= push
        -- Istore im -> pop >>= store im
        Ireturn -> do
            i <- pop >>= want_integer
            leave
            push (Integer i)
        Return -> leave
        Invokestatic c -> do
            (mcls, mname, msig) <- cp_get c >>= cp_want_methodref
            target_class <- load_class mcls
            target_method <- get_method target_class mname msig
            let nargs = length (Z.s_arg_types msig)
            args <- reverse <$> M.replicateM nargs pop
            enter (f_new target_class target_method)
            M.zipWithM_ (\ i v -> store i v) [0..] args
            -- FIXME this assume mcls == c_name cls
            -- error $ "Invokestatic: " ++ show target_method
            -- M.when (is_static c)
            -- FIXME ensure that method is:
            -- static
            -- not abstract
            -- not special (as defined by invokespecial)
            -- TODO initialize
            -- TODO synchronized
            -- TODO native
        _ -> stop $ Unknown_instruction instruction
    where

        pop_integer = pop >>= want_integer

        add_pc_if :: Bool -> Int16 -> S ()
        add_pc_if cond ofs = M.when cond $ add_pc ofs

        add_pc :: Int16 -> S ()
        add_pc ofs = modify $ \ s ->
            let
                frame = s_frame s
            in
                s { s_frame = frame { f_pc = f_pc frame + fromIntegral (fromIntegral ofs :: Pc_offset) } }

        want_integer :: Value -> S Int32
        want_integer (Integer x) = return x
        want_integer v = stop (Wrong_type Z.Int v)

        want_long :: Value -> S Int64
        want_long (Long x) = return x
        want_long v = stop (Wrong_type Z.Long v)

        want_float :: Value -> S Float
        want_float (Float x) = return x
        want_float v = stop (Wrong_type Z.Float v)

        want_double :: Value -> S Double
        want_double (Double x) = return x
        want_double v = stop (Wrong_type Z.Double v)

        iconst :: Int32 -> S ()
        iconst v = push (Integer v)

-- * Non-IO state manipulators

-- Grossly inefficient.
get_static_field :: Class_name -> Fieldref -> S Value
get_static_field class_name fref = do
    s <- get
    c <- want_any (Class_not_found class_name)
        [ c | c <- s_classes s, c_name c == class_name ]
    want_any (Field_not_initialized class_name fref) $ kv_get fref (c_static c)

want_any :: Status -> [a] -> S a
want_any _ (x : _) = return x
want_any failstat _ = stop failstat

set_static_field :: Class_name -> Fieldref -> Value -> S ()
set_static_field class_name fref value = modify $ \ s ->
    s { s_classes = map set (s_classes s) }
    where
        set c | c_name c == class_name = c { c_static = kv_upsert fref value (c_static c) }
        set c = c

data Fieldref
    = Mk_fieldref Field_name Z.Type
    deriving (Read, Show, Eq)

load_class :: Class_name -> S Class
load_class name = do
    s <- get
    -- XXX
    case [ c | c <- s_classes s, c_name c == name ] of
        x : _ -> return x
        _ -> stop (Class_not_found name)

get_field :: Class -> Field_name -> Z.Type -> S Field
get_field clas fname ftype =
    case [ f | f <- c_fields clas, f_name f == fname, f_type f == ftype ] of
        x : _ -> return x
        _ -> stop (Field_not_found (c_name clas) (Mk_fieldref fname ftype))

get_method :: Class -> Method_name -> Z.Signature -> S Method
get_method clas mname msig =
    case [ m | m <- c_methods clas, m_name m == mname, m_signature m == msig ] of
        x : _ -> return x
        _ -> stop (Method_not_found (c_name clas) mname msig)

stop :: Status -> S a
stop r = modify (stop_raw r) >> Mk_s (\ s -> (s, Nothing))

enter :: Frame -> S ()
enter new_frame = modify $ \ s ->
    let
        old_frame = s_frame s
    in
        s { s_frame = new_frame, s_frames = old_frame : s_frames s }

leave :: S ()
leave = modify $ \ s ->
    case s_frames s of
        f : g -> s { s_frame = f, s_frames = g }
        [] -> stop_raw End_of_program s

stop_raw :: Status -> State -> State
stop_raw reason s = s { s_status = reason }

-- * Accessing constant pool entries

{- |
The index starts from one.
-}
cp_get :: Word16 -> S Constant
cp_get i = do
    state <- get
    let
        frame = s_frame state
        clas = f_class frame
    maybe
        (stop Invalid_constant_pool_index)
        return
        (c_pool clas `Z.at` (fromIntegral i - 1))

cp_want_fieldref :: Constant -> S (Class_name, Field_name, Z.Type)
cp_want_fieldref (C_fieldref a b c) = return (a, b, c)
cp_want_fieldref _ = stop Expecting_fieldref

cp_want_methodref :: Constant -> S (Class_name, Method_name, Z.Signature)
cp_want_methodref (C_methodref a b c) = return (a, b, c)
cp_want_methodref _ = stop Expecting_methodref

-- * Manipulating local variable array

{- |
Get the element at the given index from
the local variable array of the current frame.
-}
load :: Local_index -> S Value
load i = do
    s <- get
    let
        frame = s_frame s
        local = f_local frame
    maybe (stop Invalid_local_index) return (local `Z.at` i)

{- |
Set the element at the given index in the local variable array
of the current frame.
-}
store :: Local_index -> Value -> S ()
store i v = modify $ \ s ->
    let
        frame = s_frame s
        local = f_local frame
    in
        s { s_frame = frame { f_local = replace i v local } }
    where
        replace :: Int -> Value -> [Value] -> [Value]
        replace index x [] | index <= 0 = [x]
        replace index x (_ : z) | index <= 0 = x : z
        replace index x [] = Padding : replace (index - 1) x []
        replace index x (y : z) = y : replace (index - 1) x z

-- | Local variable array index begins from 0.
type Local_index = Int

-- * Manipulating operand stack

push :: Value -> S ()
push v = modify $ \ s ->
    let
        frame = s_frame s
        stack = f_stack frame
    in
        s { s_frame = frame { f_stack = v : stack } }

pop :: S Value
pop = Mk_s $ \ s ->
    let
        frame = s_frame s
        stack = f_stack frame
    in
        case stack of
            [] -> (stop_raw Operand_stack_underflow s, Nothing)
            x : y -> (s { s_frame = frame { f_stack = y } }, Just x)

-- * For debugging by human programmers

-- ** Disassembly

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

-- ** Dumping state

{- |
Dump the 'State' in a way that humans can read easily.
-}
dump :: State -> String
dump state =
    unlines $
        [
            "status:"
            , ""
            , show (s_status state)
            , ""
            , "pc: " ++ show (f_pc frame)
            , ""
            , ""
                ++ bool (is_public method) "" "public "
                ++ bool (is_static method) "" "static "
                ++ pretty_return_type (Z.s_return_type signature)
                ++ " " ++ Bu.toString (m_name method)
                ++ " (" ++ L.intercalate ", " (map pretty_type $ Z.s_arg_types signature) ++ ")"
            , ""
            , disassembly
            , ""
            , "operand stack:"
            , ""
            , show (f_stack frame)
        ]
    where
        bool c f t = if c then t else f
        frame = s_frame state
        clas = f_class frame
        method = f_method frame
        pc = f_pc frame
        signature = m_signature method
        pretty_type t = case t of
            Z.Byte -> "byte"
            Z.Short -> "short"
            Z.Int -> "int"
            Z.Long -> "long"
            Z.Array u -> pretty_type u ++ "[]"
            Z.Instance c -> c
            _ -> show t
        pretty_return_type = maybe "void" pretty_type
        disassembly =
            unlines $ zipWith (\ n i -> align n ++ "    " ++ i) numbers instructions
            where
                (_, ins) = disassemble clas method
                (numbers, instructions) = (map (show . fst) ins, map (show . snd) ins)
                left_column_width = maximum (map length numbers)
                align x = replicate (left_column_width - length x) ' ' ++ x

-- * Associative lists

{- |
@kv_upsert k v x@ replaces the first occurrence of @(k,_)@ in @x@ with @(k,v)@,
or inserts @(k,v)@ if there is no such occurrence.
-}
kv_upsert :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
kv_upsert k v [] = [(k, v)]
kv_upsert k v ((j, _) : r) | k == j = (k, v) : r
kv_upsert k v (_ : r) = kv_upsert k v r

{- |
@kv_get k x@ gets the first occurrence of @(k,v)@ in @x@, for any @v@.
-}
kv_get :: (Eq k) => k -> [(k, v)] -> [v]
kv_get k = map snd . filter (\ (j, _) -> k == j)

-- * Internals

data State
    = Mk_state
    {
        s_status :: Status
        , s_frame :: Frame -- ^ current frame (topmost frame in the frame stack)
        , s_frames :: [Frame] -- ^ the other frames in the frame stack
        , s_classes :: [Class] -- ^ loaded classes
    }
    deriving (Read, Show)

{- |
When an instruction needs 'IO' but the execution happens in 'S',
the machine throws a 'Need_io' and rewinds the program counter
to point to that faulting instruction so that
resuming the machine in J retries that instruction.
-}
data Status
    = Ready
    | End_of_program
    | Operand_stack_underflow
    | Invalid_constant_pool_index
    | Invalid_local_index
    | Method_lacks_Code
    | Invalid_pc Pc
    | Decode_error Word8
    | Unknown_instruction Instruction
    | Need_io Instruction
    | Wrong_type Z.Type Value -- ^ expected-type actual-value ; value has wrong type
    | Expecting_fieldref
    | Expecting_methodref
    | Class_not_found Class_name
    | Field_not_found Class_name Fieldref
    | Field_not_initialized Class_name Fieldref
    | Method_not_found Class_name Method_name Z.Signature
    | Invalid_tableswitch String
    | Invalid_lookupswitch String
    | Invalid_reserved Int Word8
    | Not_implemented
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
    deriving (Read, Show)

{- |
Program counter type.

'Pc' must be unsigned,
'Pc_offset' must be signed,
and they must have the same width.
-}
type Pc = Word32

type Pc_offset = Int32

-- * Internals: dereferencing constant pool

{- |
This transforms a 'Z.Class' into a 'Class'
by dereferencing the constant pool.
-}
resolve_class :: Z.Class -> Either String Class
resolve_class c = do
    Mk_class
    <$> (Z.cp_get_class c (Z.c_this c) >>= Z.cp_get_utf8 c)
    <*> fields
    <*> methods
    <*> resolve_constant_pool c
    <*> pure []
    where
        fields =
            M.forM (Z.c_fields c) $ \ f -> do
                name <- Z.cp_get_utf8 c (Z.fi_name f)
                type_ <- Z.cp_get_utf8 c (Z.fi_descriptor f) >>= Z.parse_field_type
                attrs <- mapM (resolve_attribute c) (Z.fi_attributes f) -- XXX not yet used
                return $ Mk_field (Z.fi_access f) name type_
        methods =
            M.forM (Z.c_methods c) $ \ m -> do
                name <- Z.cp_get_utf8 c (Z.mi_name m)
                type_ <- Z.cp_get_utf8 c (Z.mi_descriptor m) >>= Z.parse_method_type
                attrs <- mapM (resolve_attribute c) (Z.mi_attributes m)
                let contents = [ c | Mk_attribute name c <- attrs
                        , name == Bu.fromString "Code" ]
                    mb_content = case contents of
                        x : _ -> Just x
                        _ -> Nothing
                mb_code <- case mb_content of
                    Nothing -> Right Nothing
                    Just content -> Just <$> Z.parse_code_attr_content content
                return $ Mk_method (Z.mi_access m) name type_ mb_code

resolve_attribute :: Z.Class -> Z.Attribute -> Either String Attribute
resolve_attribute c a =
    Mk_attribute
        <$> Z.cp_get_utf8 c (Z.a_name a)
        <*> pure (Z.a_content a)

resolve_constant_pool :: Z.Class -> Either String [Constant]
resolve_constant_pool c =
    mapM resolve_entry (Z.c_pool c)
    where
        resolve_entry e = case e of
            Z.P_utf8 a -> Right $ C_string a
            Z.P_class i -> C_string <$> Z.cp_get_utf8 c i
            Z.P_string i -> C_string <$> Z.cp_get_utf8 c i
            Z.P_fieldref i_c i_nt -> do
                cname <- Z.cp_get_class c i_c >>= Z.cp_get_utf8 c
                (i_fname, i_ftype) <- Z.cp_get_nameandtype c i_nt
                fname <- Z.cp_get_utf8 c i_fname
                ftype <- Z.cp_get_utf8 c i_ftype >>= Z.parse_field_type
                return $ C_fieldref cname fname ftype
            Z.P_methodref i_c i_nt -> do
                cname <- Z.cp_get_class c i_c >>= Z.cp_get_utf8 c
                (i_mname, i_mtype) <- Z.cp_get_nameandtype c i_nt
                mname <- Z.cp_get_utf8 c i_mname
                mtype <- Z.cp_get_utf8 c i_mtype >>= Z.parse_method_type
                return $ C_methodref cname mname mtype
            Z.P_interfacemethodref i_c i_nt -> Right C_resolved -- FIXME
            Z.P_nameandtype _ _ -> Right C_resolved
