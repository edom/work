{- |
This concern of this module is the execution of codes in class files.
-}
module Jvm_execute
where

import Prelude ()
import Meta.Prelude

import qualified Data.ByteString.UTF8 as Bu

import qualified Meta.List as Li

import Meta.JvmArch -- XXX
import Jvm_decode (decode)
import Meta.JvmIns
    (
        Instruction(..)
    )
import Meta.JvmMember
    (
        Field_ref(..)
        , Method_name
    )
import qualified Meta.JvmArch as A
import qualified Jvm_constant as C
import qualified Jvm_decode as D
import qualified Jvm_load as L
import qualified Meta.JvmType as T
import qualified Meta.JvmTys as U
import qualified Meta.JvmValue as V

-- * Initializing classes

{- |
This initializes only the class and not the superclasses.
-}
init_class :: (Execute m) => A.Class -> m ()
init_class cls = do
    s <- A.get
    mm <- find_class_initializer cls
    case mm of
        Just m -> do
            A.begin_call cls m []
            _ <- step_until_returned
            return ()
        _ ->
            return ()
    where
        find_class_initializer c =
            A.find_method c
                (Bu.fromString "<clinit>")
                (T.Mk_signature [] T.Void)

resolve_method :: (Execute m) => Class -> Method_name -> T.Signature -> m (Class, Method)
resolve_method orig_cls mname msig = loop orig_cls
    where
        loop cls = do
            -- FIXME 5.4.3.3
            m0 <- find_method cls mname msig
            case m0 of
                Just m -> return (cls, m)
                Nothing -> do
                    case c_super cls of
                        Just sup -> do
                            s <- load_and_init_class sup
                            loop s
                        Nothing -> stop (Method_not_found (c_name orig_cls) mname msig)

{- |
Load, initialize, and register the class
and all its superclasses (but not interfaces).

In this module, you should use this function
instead of using 'L.load_class' directly.
-}
load_and_init_class :: (Execute m) => A.Class_name -> m Class
load_and_init_class = loop 0
    where
        max_depth = 32
        loop d _ | d >= max_depth = stop (Hierarchy_too_deep d)
        loop d name = do
            -- Should we also load, initialize, and register the class's interfaces?
            cls <- L.load_class name
            case A.c_initialized cls of
                True ->
                    return cls
                _ -> do
                    -- XXX: should we have Uninitialized, Initializing, and Initialized?
                    case c_super cls of
                        Nothing -> return ()
                        Just super -> loop (d + 1) super >> return ()
                    let cls1 = cls { A.c_initialized = True }
                    A.add_loaded_class cls1
                    init_class cls1
                    A.get_loaded_class name

-- * Bytecode execution

-- ** Executing an instruction

{- |
The smallest execution step.

Decode the next instruction and execute it.

The program counter is incremented accordingly.
-}
step :: (Execute m) => m ()
step = do
    s <- get
    f <- get_frame
    let
        c = f_class f
        m = f_method f
        b = m_body m
        rtype = T.s_return_type (m_signature m)
    case b of
        Missing -> do
            b1 <- A.find_binding (A.c_name c) (A.m_name m) (A.m_signature m)
            case b1 of
                Nothing -> stop Method_body_missing
                Just m1 -> case A.m_body m1 of
                    Missing -> stop Method_body_missing
                    Bytecode _ -> decode >>= execute
                    Native e -> do
                        val <- execute_native e
                        leave
                        push val
                    Native_io e -> do
                        val <- execute_native_io e
                        leave
                        push val
        Bytecode _ -> decode >>= execute
        Native e -> do
            val <- execute_native e
            leave
            push val
        Native_io e -> do
            val <- execute_native_io e
            leave
            push val

{- |
This runs the method until it returns,
and then this returns that method's return value
(pops it from the operand stack).
-}
step_until_returned :: (Execute m) => m V.Value
step_until_returned = do
    state <- get
    depth <- get_depth
    frame <- get_frame
    let
        rtype = T.s_return_type . m_signature $ f_method frame
        loop = do
            step
            d <- get_depth
            if d >= depth
                then loop
                else pop_return_value rtype
    loop
    where
        get_depth = length . s_frames <$> get

{- |
Execute an instruction.

The program counter is incremented accordingly.
-}
execute :: (Execute m) => Instruction -> m ()
execute instruction =
    -- XXX should use vector (or Map Int) instead of list
    case instruction of
        Nop -> return ()
        Aconst_null -> push V.Null
        Iconst_m1 -> iconst (negate 1)
        Iconst_0 -> iconst 0
        Iconst_1 -> iconst 1
        Iconst_2 -> iconst 2
        Iconst_3 -> iconst 3
        Iconst_4 -> iconst 4
        Iconst_5 -> iconst 5
        Lconst_0 -> lconst 0
        Lconst_1 -> lconst 1
        Fconst_0 -> fconst 0
        Fconst_1 -> fconst 1
        Fconst_2 -> fconst 1
        Dconst_0 -> dconst 0
        Dconst_1 -> dconst 1
        Bipush b -> push (V.Integer $ fromIntegral b)
        Sipush s -> push (V.Integer $ fromIntegral s)
        Ldc c -> C.get (fromIntegral c) >>= constant_to_value >>= push
        Ldc_w c -> C.get (fromIntegral c) >>= constant_to_value >>= push
        Ldc2_w c -> C.get (fromIntegral c) >>= constant_to_value >>= push
        -- TODO check type
        Iload n -> load (fromIntegral n) >>= push
        Lload n -> load (fromIntegral n) >>= push
        Fload n -> load (fromIntegral n) >>= push
        Dload n -> load (fromIntegral n) >>= push
        Aload n -> load (fromIntegral n) >>= push
        Iload_0 -> load 0 >>= push
        Iload_1 -> load 1 >>= push
        Iload_2 -> load 2 >>= push
        Iload_3 -> load 3 >>= push
        Lload_0 -> load 0 >>= push
        Lload_1 -> load 1 >>= push
        Lload_2 -> load 2 >>= push
        Lload_3 -> load 3 >>= push
        Fload_0 -> load 0 >>= push
        Fload_1 -> load 1 >>= push
        Fload_2 -> load 2 >>= push
        Fload_3 -> load 3 >>= push
        Dload_0 -> load 0 >>= push
        Dload_1 -> load 1 >>= push
        Dload_2 -> load 2 >>= push
        Dload_3 -> load 3 >>= push
        Aload_0 -> load 0 >>= push
        Aload_1 -> load 1 >>= push
        Aload_2 -> load 2 >>= push
        Aload_3 -> load 3 >>= push
        Iaload -> xaload
        Laload -> xaload
        Faload -> xaload
        Daload -> xaload
        Aaload -> xaload
        Baload -> xaload
        Caload -> xaload
        Saload -> xaload
        -- Lload i -> get_local i >>= push
        -- TODO check type first
        Istore n -> pop >>= store (fromIntegral n)
        Lstore n -> pop >>= store (fromIntegral n)
        Fstore n -> pop >>= store (fromIntegral n)
        Dstore n -> pop >>= store (fromIntegral n)
        Astore n -> pop >>= store (fromIntegral n)
        Istore_0 -> pop_integer >>= store 0 . V.Integer
        Istore_1 -> pop_integer >>= store 1 . V.Integer
        Istore_2 -> pop_integer >>= store 2 . V.Integer
        Istore_3 -> pop_integer >>= store 3 . V.Integer
        Lstore_0 -> pop_long >>= store 0 . V.Long
        Lstore_1 -> pop_long >>= store 1 . V.Long
        Lstore_2 -> pop_long >>= store 2 . V.Long
        Lstore_3 -> pop_long >>= store 3 . V.Long
        -- TODO check type
        Astore_0 -> pop >>= store 0
        Astore_1 -> pop >>= store 1
        Astore_2 -> pop >>= store 2
        Astore_3 -> pop >>= store 3
        Castore -> do
            -- FIXME
            value <- pop
            V.Integer index_ <- pop
            V.Array T.Char n r <- pop
            A.modify_ioref r (Li.replace (U.def_value T.Char) (fromIntegral index_) value)
        Iastore -> do
            -- FIXME
            value <- pop
            V.Integer index_ <- pop
            V.Array T.Int n r <- pop
            A.modify_ioref r (Li.replace (U.def_value T.Char) (fromIntegral index_) value)
        {-
        Iastore -> do
            value_ <- pop
            index_ <- pop
            array_ <- pop
            index <- case value_ of
                V.Integer a -> return (fromIntegral a :: Int) -- XXX
                _ -> stop (Type_must_be (U.type_of value_) T.Int)
            case array_ of
                V.Array elemtype@T.Int capacity ioref ->
                    case () of
                        _ | not (0 <= index && index < fromIntegral capacity) -> stop Array_index_out_of_bounds
                        _ -> modify_ioref ioref (Li.replace (U.def_value elemtype) index value_)
                V.Null -> stop Unexpected_null
                _ -> stop Expecting_array
        -}
        Pop -> pop >> return ()
        Pop2 -> pop >> return ()
        Dup -> peek >>= push
        Iadd -> binop (+) want_integer V.Integer
        Ladd -> binop (+) want_long V.Long
        Fadd -> binop (+) want_float V.Float
        Dadd -> binop (+) want_double V.Double
        Isub -> binop (-) want_integer V.Integer
        Lsub -> binop (-) want_long V.Long
        Fsub -> binop (-) want_float V.Float
        Dsub -> binop (-) want_double V.Double
        Imul -> binop (*) want_integer V.Integer
        Lmul -> binop (*) want_long V.Long
        Fmul -> binop (*) want_float V.Float
        Dmul -> binop (*) want_double V.Double
        Ishl -> do
            value2 <- fromIntegral <$> pop_integer
            value1 <- pop_integer
            push $ V.Integer $ value1 `shiftL` value2
        Iushr -> do
            value2 <- fromIntegral <$> pop_integer
            value1 <- pop_integer
            -- XXX shiftR is not well-defined if value2 is negative
            push $ V.Integer $ fromIntegral $ (fromIntegral value1 :: Word32) `shiftR` value2
        Lushr -> do
            value2 <- pop_integer
            value1 <- pop_long
            -- XXX shiftR is not well-defined if value2 is negative
            let result = fromIntegral ((fromIntegral value1 :: Word64) `shiftR` (fromIntegral value2 :: Int)) :: Int64
            push (V.Long result)
        Iand -> binop (.&.) want_integer V.Integer
        Land -> binop (.&.) want_long V.Long
        Ior -> binop (.|.) want_integer V.Integer
        Lor -> binop (.|.) want_long V.Long
        Ixor -> binop xor want_integer V.Integer
        Lxor -> binop xor want_long V.Long
        Iinc i_ c ->
            let
                i = fromIntegral i_
            in
                load i
                >>= want_integer
                >>= store i . V.Integer . (fromIntegral c +)
        I2l -> pop_integer >>= push . V.Long . fromIntegral
        I2f -> pop_integer >>= push . V.Float . fromIntegral
        L2i -> pop_long >>= push . V.Integer . fromIntegral
        F2i -> do
            x <- pop_float
            push $ V.Integer $ case () of
                _ | isNaN x -> 0
                _ -> truncate x -- TODO test if this rounds towards zero
        Lcmp -> do
            value2 <- pop_long
            value1 <- pop_long
            push $ V.Integer $ case () of
                _ | value1 < value2 -> negate 1
                _ | value1 > value2 -> 1
                _ -> 0
        Fcmpl -> do
            value2 <- pop_float
            value1 <- pop_float
            push $ V.Integer $ case () of
                _ | isNaN value1 || isNaN value2 -> negate 1
                _ | value1 < value2 -> negate 1
                _ | value1 > value2 -> 1
                _ -> 0
        Fcmpg -> do
            value2 <- pop_float
            value1 <- pop_float
            push $ V.Integer $ case () of
                _ | isNaN value1 || isNaN value2 -> 1
                _ | value1 < value2 -> negate 1
                _ | value1 > value2 -> 1
                _ -> 0
        Dcmpl -> do
            value2 <- pop_double
            value1 <- pop_double
            push $ V.Integer $ case () of
                _ | isNaN value1 || isNaN value2 -> negate 1
                _ | value1 < value2 -> negate 1
                _ | value1 > value2 -> 1
                _ -> 0
        Dcmpg -> do
            value2 <- pop_double
            value1 <- pop_double
            push $ V.Integer $ case () of
                _ | isNaN value1 || isNaN value2 -> 1
                _ | value1 < value2 -> negate 1
                _ | value1 > value2 -> 1
                _ -> 0
        Ifeq ofs -> if_ (==) ofs
        Ifne ofs -> if_ (/=) ofs
        Iflt ofs -> if_ (<) ofs
        Ifge ofs -> if_ (>=) ofs
        Ifgt ofs -> if_ (>) ofs
        Ifle ofs -> if_ (<=) ofs
        If_icmpeq ofs -> if_icmp (==) ofs
        If_icmpne ofs -> if_icmp (/=) ofs
        If_icmplt ofs -> if_icmp (<) ofs
        If_icmpge ofs -> if_icmp (>=) ofs
        If_icmpgt ofs -> if_icmp (>) ofs
        If_icmple ofs -> if_icmp (<=) ofs
        -- Iload im -> get_local im >>= push
        -- Istore im -> pop >>= store im
        Getstatic c -> do
            (cname, fname, ftype) <- C.get_field_ref c
            target_class <- load_and_init_class cname
            read_static_field target_class (Mk_field_ref fname ftype) >>= push
        Putstatic c -> do
            (cname, fname, ftype) <- C.get_field_ref c
            target_class <- load_and_init_class cname
            pop >>= write_static_field target_class (Mk_field_ref fname ftype)
        Getfield c -> do
            (_, fname, ftype) <- C.get_field_ref c
            ins <- pop
            val <- read_instance_field ins (Mk_field_ref fname ftype)
            push val
        Putfield c -> do
            (_, fname, ftype) <- C.get_field_ref c
            val <- pop
            ins <- pop
            write_instance_field ins (Mk_field_ref fname ftype) val
        Goto ofs -> add_pc (ofs - 3)
        Ireturn -> xreturn
        Lreturn -> xreturn
        Freturn -> xreturn
        Dreturn -> xreturn
        Areturn -> xreturn
        Return -> leave
        Invokestatic c -> do
            (mcls, mname, msig) <- C.get_method_ref c
            let nargs = length (T.s_arg_types msig)
            args <- reverse <$> replicateM nargs pop
            cls <- load_and_init_class mcls
            met <- A.get_method cls mname msig
            A.begin_call cls met args
            -- FIXME this assume mcls == c_name cls
            -- error $ "Invokestatic: " ++ show target_method
            -- when (is_static c)
            -- FIXME ensure that method is:
            -- static
            -- not abstract
            -- not special (as defined by invokespecial)
            -- TODO initialize
            -- TODO synchronized
            -- TODO native
        Invokevirtual c -> do
            -- FIXME
            (_, mname, msig) <- C.get_method_ref c
            let nargs = 1 + length (T.s_arg_types msig)
            -- XXX
            this@(V.Instance cname _) : args <- reverse <$> replicateM nargs pop
            origin_class <- load_and_init_class cname
            (target_class, target_method) <- resolve_method origin_class mname msig
            begin_call target_class target_method (this : args)
        Invokespecial c -> do
            -- FIXME
            (mcls, mname, msig) <- C.get_method_ref c
            target_class <- load_and_init_class mcls
            target_method <- get_method target_class mname msig
            let nargs = 1 + length (T.s_arg_types msig)
            args <- reverse <$> replicateM nargs pop
            begin_call target_class target_method args
        New c -> C.get_class_name c >>= new >>= push
        Newarray b -> do
            elemtype <- type_from_newarray_byte b
            capacity <- pop_integer
            new_array elemtype capacity [] >>= push
        Anewarray b -> do
            elemtype <- C.get_class_name b
            capacity <- pop_integer
            new_array (T.Instance elemtype) capacity [] >>= push
        Arraylength -> do
            v <- pop
            case v of
                V.Array _ n _ -> push (V.Integer n)
                V.Null -> stop Unexpected_null
                _ -> stop Expecting_array
        Athrow -> do
            -- XXX
            val <- pop
            let msg = T.pretty (U.type_of val)
            stop (Unhandled_exception msg)
        Instanceof c -> do
            sup <- C.get_class_name c
            obj <- pop
            val <- case obj of
                V.Null -> return 0
                V.Instance cls _ -> do
                    boolify <$> is_subclass_of cls sup
                -- TODO implement https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.instanceof
                _ -> return 0
            push $ V.Integer val
        Monitorenter -> return () -- TODO implement
        Monitorexit -> return () -- TODO implement
        Ifnull ofs -> do
            v <- pop
            add_pc_if (V.is_null v) (ofs - 3)
        Ifnonnull ofs -> do
            v <- pop
            add_pc_if (not $ V.is_null v) (ofs - 3)
        _ -> stop $ Not_implemented $ show instruction

    where

        is_subclass_of name0 name1 | name0 == name1 = return True
        is_subclass_of name0 name1 = do
            cls0 <- load_and_init_class name0
            case c_super cls0 of
                Nothing -> return False
                Just sup0 -> is_subclass_of sup0 name1
            -- TODO c_interfaces

        boolify False = 0
        boolify True = 1

        xaload = do
            index <- pop_integer
            (t, n, r) <- pop_array
            unless (0 <= index && index < n) $ stop Array_index_out_of_bounds
            list <- read_ioref r
            push $ maybe (U.def_value t) id (list `Li.at` fromIntegral index)

        xreturn = do
            x <- pop
            leave
            push x

        binop op want mkval = do
            value2 <- pop >>= want
            value1 <- pop >>= want
            push $ mkval $ op value1 value2

        -- The offset in control instructions is relative to the beginning of the instruction.
        if_ cmp ofs = do
            v <- pop
            i <- case v of
                V.Bool x -> return $ fromIntegral x -- XXX sign or zero extend?
                V.Byte x -> return $ fromIntegral x -- XXX sign or zero extend?
                V.Short x -> return $ fromIntegral x
                V.Integer x -> return x
                _ -> stop (Type_must_be (U.type_of v) T.Int)
            add_pc_if (cmp i 0) (ofs - 3)
        if_icmp cmp ofs = do
            x <- pop >>= want_integer
            y <- pop >>= want_integer
            when (cmp y x) $ add_pc (ofs - 3)
        iconst = push . V.Integer
        lconst = push . V.Long
        fconst = push . V.Float
        dconst = push . V.Double
        add_pc ofs = modify_frame $ \ frame ->
            frame { f_pc = f_pc frame + fromIntegral (fromIntegral ofs :: Pc_offset) }
        add_pc_if cond ofs = when cond $ add_pc (ofs :: Int16)

        pop_integer = pop >>= want_integer
        pop_long = pop >>= want_long
        pop_float = pop >>= want_float
        pop_double = pop >>= want_double
        pop_array = pop >>= want_array

        want_array (V.Array t n x) = return (t, n, x)
        want_array V.Null = stop Unexpected_null
        want_array v = stop Expecting_array

        want_integer v = maybe (v `type_must_be` T.Int) return (V.integer v)
        want_long v = maybe (v `type_must_be` T.Long) return (V.long v)
        want_float v = maybe (v `type_must_be` T.Float) return (V.float v)
        want_double v = maybe (v `type_must_be` T.Double) return (V.double v)

        type_must_be v t = stop (Type_must_be (U.type_of v) t)

{- |
This helps decoding the byte parameter of the 'Newarray' instruction.
-}
type_from_newarray_byte :: (A.Stateful m) => Word8 -> m T.Type
type_from_newarray_byte a = case a of
    4 -> return T.Bool
    5 -> return T.Char
    6 -> return T.Float
    7 -> return T.Double
    8 -> return T.Byte
    9 -> return T.Short
    10 -> return T.Int
    11 -> return T.Long
    _ -> A.stop (A.Invalid_newarray_type a)

-- init_instance :: V.Value -> [V.Value] ->

call
    :: (Execute m)
    => Class_name
    -> Method_name
    -> T.Signature
    -> [V.Value]
    -> m V.Value

call cname mname msig args = do
    cls <- load_and_init_class cname
    met <- A.get_method cls mname msig
    A.begin_call cls met args
    step_until_returned

class (A.Stateful m, D.Fetch m, L.Load m) => Execute m where

    execute_native :: S a -> m a

    execute_native_io :: J a -> m a
    execute_native_io _ = stop Need_io

    {- |
This allocates an instance but does not initialize it.

See also 'V.Instance' in "Jvm_value".

To initialize an instance, 'call' one of its constructors (@<init>@ methods).
    -}
    new :: Class_name -> m V.Value
    new _ = stop Need_io

    {- |
This allocates an array and sets its elements according to the list.
    -}
    new_array :: T.Type -> Int32 -> [V.Value] -> m V.Value
    new_array _ _ _ = stop Need_io

instance Execute S where

    execute_native = id

instance Execute J where

    execute_native = lift

    execute_native_io = id

    new cname = do
        _ <- load_and_init_class cname
        V.Instance cname <$> liftIO (newIORef [])

    -- TODO If count is less than zero, newarray throws a NegativeArraySizeException.
    -- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.newarray

    new_array _ capacity _ | capacity < 0 = stop Negative_array_size
    new_array elemtype capacity initvals =
        V.Array elemtype capacity <$> liftIO (newIORef initvals)

-- * Creating instances of core classes

-- String (char[], int, int)
new_String :: (Execute m) => String -> m V.Value
new_String str = do
    cha <- new_char_array str
    ins <- new cname
    _ <- call cname (Bu.fromString "<init>")
        (T.Mk_signature [T.Array T.Char, T.Int, T.Int] T.Void)
        [ins, cha, V.Integer 0, V.Integer (fromIntegral $ length str)]
    return ins
    where
        cname = Bu.fromString "java/lang/String"

-- Class (ClassLoader)
new_Class :: (Execute m) => m V.Value
new_Class = do
    ins <- new cname
    _ <- call cname (Bu.fromString "<init>")
        (T.Mk_signature [T.Instance (Bu.fromString "java/lang/ClassLoader")] T.Void)
        [ins, V.Null] -- XXX
    return ins
    where
        cname = Bu.fromString "java/lang/Class"

new_char_array :: (Execute m) => String -> m V.Value
new_char_array str = do
    new_array T.Char strlen (V.jchar_list_from_string str)
    where
        strlen = fromIntegral (length str) -- XXX can truncate Int to Int32?

-- * Turning 'Constant' into 'V.Value'

constant_to_value :: (Execute m) => Constant -> m V.Value
constant_to_value c = case c of
    C_integer a -> return (V.Integer a)
    C_float a -> return (V.Float a)
    C_long a -> return (V.Long a)
    C_double a -> return (V.Double a)
    C_string bs -> new_String (Bu.toString bs)
    C_class n -> new_Class
    _ -> error $ "Jvm_execute.constant_to_value: not implemented: " ++ show c
