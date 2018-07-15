{- |
* The approach here is to describe instructions.

* The approach in "Meta.JvmIns" is to make a big data-type.

* It should be possible to generate "Meta.JvmIns" using this module.
-}
module Meta.JvmIns0 (
    -- * Parsing
    parse
    , Val(..)
    , Parse(..)
    -- * How to describe an instruction
    , Ins(..)
    , Name
    , Pat
    -- * Instruction descriptions
    , instructions
) where

import Prelude (mod)
import Meta.Prelude

-- | This describes a JVM instruction.
data Ins
    = MkIns {
        _name :: Name -- ^ mnemonic; lowercase
        , _pattern :: Pat -- ^ layout in memory; for reading and writing
    } deriving (Read, Show)

-- | Instruction mnemonic.
-- Should consist of lowercase characters only.
type Name = String

-- | 1-byte instruction.
simple :: Name -> Word8 -> Ins
simple name opcode = MkIns name (Fix opcode)

{- |
This describes how the instruction is laid out in memory.

This should be able to describe variable-length instructions such as @tableswitch@ and @lookupswitch@.
-}
data Pat
    = Emp -- ^ empty
    | Fix Word8 -- ^ fixed byte (always the same)
    | PInt8 -- ^ 8-bit signed integer
    | PWord16 -- ^ 16-bit big-endian unsigned integer
    | Align4 -- ^ discard bytes until program counter is divisible by 4
    | Seq Pat Pat -- ^ Seq x y is x immediately followed by y
    deriving (Read, Show)

instance Semigroup Pat where
    (<>) = Seq

-- | For 'parse'.
class (Monad m) => Parse m where

    -- | Get the program counter.
    get_pc :: m Word16

    -- | Get the byte pointed by the program counter, and then increment the program counter.
    fetch :: m Word8

    -- | End parsing with failure.
    mismatch :: m a

    -- | Discard bytes until program counter is divisible by 4.
    align4 :: m ()
    align4 = do
        pc <- get_pc
        unless (pc `mod` 4 == 0) (fetch >> align4)

-- | Value.
data Val
    = VWord8 Word8
    | VWord16 Word16
    | VInt8 Int8
    deriving (Read, Show)

-- | Parse the input according to the 'Pat' pattern.
-- The input is implied in @m@.
parse :: (Parse m) => Pat -> m [Val]
parse pat = case pat of
    Emp -> return []
    Fix a -> do
        b <- fetch
        if a == b
            then return [VWord8 a]
            else mismatch
    -- We assume that fromIntegral doesn't change the bit pattern.
    PInt8 -> return . VInt8 . fromIntegral <$> fetch
    PWord16 -> do
        b1 <- fetch
        b0 <- fetch
        return [VWord16 $ (fromIntegral b1 `shiftL` 8) .|. fromIntegral b0]
    Align4 -> align4 >> return []
    Seq a b -> (++) <$> parse a <*> parse b

-- | All instruction descriptions.
instructions :: [Ins]
instructions = [
        nop
        , simple "aconst_null" 1
        , simple "iconst_m1" 2
        , nop { _name = "bipush", _pattern = Fix 16 <> PInt8 }
    ]
    where
        nop = simple "nop" 0
