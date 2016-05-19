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

import qualified Data.ByteString.UTF8 as Bu

import qualified Jvm_io as Z

import Jvm_arch -- XXX
import Jvm_decode (decode)
import Jvm_prepare -- XXX

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

-- * Bytecode execution

-- ** Executing an instruction

{- |
Decode the next instruction and execute it.

The program counter is incremented accordingly.
-}
step :: S ()
step = do
    s <- get
    let
        f = s_frame s
        m = f_method f
        b = m_body m
    case b of
        Missing ->
            stop Method_body_missing
        Bytecode _ ->
            decode >>= execute
        Native e ->
            case Z.s_return_type (m_signature m) of
                Nothing ->
                    e >> leave
                _ -> do
                    r <- e
                    leave
                    push r
        Native_io _ ->
            stop Need_io

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
        want_integer v = stop (Expecting_type Z.Int)

        want_long :: Value -> S Int64
        want_long (Long x) = return x
        want_long v = stop (Expecting_type Z.Long)

        want_float :: Value -> S Float
        want_float (Float x) = return x
        want_float v = stop (Expecting_type Z.Float)

        want_double :: Value -> S Double
        want_double (Double x) = return x
        want_double v = stop (Expecting_type Z.Double)

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

enter :: Frame -> S ()
enter new_frame = modify $ \ s ->
    let
        old_frame = s_frame s
    in
        s { s_frame = new_frame, s_frames = old_frame : s_frames s }

leave :: S ()
leave = do
    s <- get
    case s_frames s of
        f : g -> put s { s_frame = f, s_frames = g }
        [] -> stop End_of_program

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
pop = do
    s <- get
    let
        frame = s_frame s
        stack = f_stack frame
    case stack of
        [] -> stop Operand_stack_underflow
        x : y -> do
            put s { s_frame = frame { f_stack = y } }
            return x

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
