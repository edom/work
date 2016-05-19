module Jvm_decode
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

import Jvm_arch
    (
        State(..)
        , Status(..)
        , Frame(..)
        , Instruction(..)
        , S(..)
        , J(..)
        , Pc
    )
import qualified Jvm_arch as A
import qualified Jvm_io as Z

-- * Decoding JVM bytecode into instructions

class Fetch m where
    -- | Halt processing with the given reason.
    stop :: Status -> m a
    -- | Get the program counter that points to the byte that will be 'fetch'ed.
    get_pc :: m Pc
    -- | Get the byte pointed by the program counter, and increment the program counter.
    fetch :: m Word8

instance Fetch S where

    stop = A.stop

    get_pc = f_pc . s_frame <$> A.get

    fetch = do
        s <- A.get
        let
            frame = s_frame s
            pc = f_pc frame
            ipc = fromIntegral pc :: Int -- FIXME what if pc doesn't fit in an Int?
            body = A.m_body . f_method . s_frame $ s
        case body of
            A.Missing -> stop Method_lacks_Code
            A.Bytecode code_ -> do
                let code = Z.cd_code code_
                case () of
                    _ | ipc < Bs.length code -> do
                        A.put s { s_frame = frame { f_pc = pc + 1 } }
                        return (Bsu.unsafeIndex code ipc)
                    _ ->
                        stop (Invalid_pc pc)
            _ -> stop Method_body_not_fetchable

instance Fetch J where
    stop = A.stop
    get_pc = A.lift get_pc
    fetch = A.lift fetch

{- |
Fetch and decode the next instruction, and increment pc accordingly.

You get 'decode' for practically every instance of 'Fetch'.
-}
decode :: (Monad m, Fetch m) => m Instruction
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
        mbz n = u1 >>= \ a -> if a == 0
            then return ()
            else stop (Invalid_reserved n a)
        -- XXX this aligns with respect to the beginning of code[]
        align4 = do
            p <- get_pc
            M.replicateM_ (fromIntegral $ 4 - mod p 4) u1
        u1 = fetch
        s1 = fromIntegral <$> u1
        u2 = do
            a <- u1
            b <- u1
            return (unsafeShiftL (fromIntegral a) 8 .|. fromIntegral b :: Word16)
        u4 = do
            a <- u1
            b <- u1
            c <- u1
            d <- u1
            return
                (
                unsafeShiftL (fromIntegral a) 24
                .|. unsafeShiftL (fromIntegral b) 16
                .|. unsafeShiftL (fromIntegral c) 8
                .|. fromIntegral d
                :: Word32
                )
        s4 = as_int32 . fromIntegral <$> u4
        s2 = as_int16 . fromIntegral <$> u2
        as_int32 x = x :: Int32
        as_int16 x = x :: Int16
        -- XXX where is the documentation for fromIntegral?
