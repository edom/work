module Meta.JvmIns (
    -- * Java Virtual Machine instruction set
    Instruction(..)
) where

import Prelude ()
import Meta.Prelude

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
    | Newarray Word8 -- ^ 188
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
