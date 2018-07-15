module Meta.JvmArch where

import Prelude ()
import Meta.Prelude

import qualified Control.Monad as M
import qualified Data.IORef as Ir
import qualified Data.List as DL

import qualified Data.ByteString as Bs

import qualified Data.ByteString.UTF8 as Bu

import qualified Meta.JvmClsConst as K
import qualified Meta.JvmMember as Me
import qualified Meta.JvmType as T
import qualified Meta.JvmTys as U
import qualified Meta.JvmValue as V
import qualified Meta.List as Li

-- * Monads: S and J

{- |
This class represents what is common between 'S' and 'J'.

This class should be renamed to @Interpret@.
-}
class (Monad m) => Stateful m where
    get :: m State
    put :: State -> m ()
    stop :: Status -> m a

    debug_call :: String -> m ()
    debug_call _ = return ()

    read_ioref :: Ir.IORef a -> m a
    read_ioref _ = stop Need_io

    modify_ioref :: Ir.IORef a -> (a -> a) -> m ()
    modify_ioref _ _ = stop Need_io

gets :: (Stateful m) => (State -> a) -> m a
gets f = f <$> get

modify :: (Stateful m) => (State -> State) -> m ()
modify f = get >>= put . f

modify_class :: (Stateful m) => Class_name -> (Class -> Class) -> m ()
modify_class name change =
    modify $ \ s -> s
        {
            s_classes = Li.update_first (\ c -> c_name c == name) change (s_classes s)
        }

mark_class_initialized :: (Stateful m) => Class_name -> m ()
mark_class_initialized name = modify_class name (\ c -> c { c_initialized = True })

{- |
An inhabitant of @'S' a@ is a computation
that can change the state of a virtual machine.
-}
newtype S a = Mk_s { un_s :: State -> (State, Maybe a) }

{- |
'J' is like 'S', but 'J' allows 'IO' while 'S' does not.

Perhaps we should generalize both S and J to something like this:

@
newtype G m a = MkG { unG :: State -> m (State, Maybe a) }
@
-}
newtype J a = Mk_j { un_j :: State -> IO (State, Maybe a) }

-- | Whatever you can do in 'IO', you can do in 'J'.
j_lift_io :: IO a -> J a
j_lift_io i = Mk_j $ \ s -> do
    a <- i
    return (s, Just a)

-- | Whatever you can do in 'S', you can do in 'J'.
lift :: S a -> J a
lift x = Mk_j (return . un_s x)

eval :: S a -> State -> Maybe a
eval comp init_state = snd $ un_s comp init_state

exec :: S a -> State -> State
exec comp init_state = fst $ un_s comp init_state

exec_io :: J a -> State -> IO State
exec_io comp init_state = fst <$> un_j comp init_state

-- * Registering loaded classes

add_loaded_class :: (Stateful m) => Class -> m ()
add_loaded_class c =
    modify $ \ s -> s { s_classes = c : s_classes s }

-- * Finding loaded classes

find_loaded_class :: (Stateful m) => Class_name -> m (Maybe Class)
find_loaded_class name = do
    s <- get
    case [ c | c <- s_classes s, c_name c == name ] of
        x : _ -> return (Just x)
        _ -> return Nothing

{- |
This is like 'find_loaded_class', but this halts the machine
if the class has not been loaded yet.
-}
get_loaded_class :: (Stateful m) => Class_name -> m Class
get_loaded_class name =
    find_loaded_class name
    >>= maybe (stop (Class_not_found name)) return

-- * Manipulating operand stack

-- | Push value to operand stack.
push :: (Stateful m) => V.Value -> m ()
push v = modify_frame $ \ f -> f { f_stack = v : f_stack f }

peek :: (Stateful m) => m V.Value
peek = do
    frame <- get_frame
    case f_stack frame of
        [] -> stop Operand_stack_underflow
        x : _ -> return x

-- | Pop value from operand stack.
pop :: (Stateful m) => m V.Value
pop = do
    frame <- get_frame
    let stack = f_stack frame
    case stack of
        [] -> stop Operand_stack_underflow
        x : y -> do
            replace_frame frame { f_stack = y }
            return x

pop_return_value :: (Stateful m) => T.Type -> m V.Value
pop_return_value t = case t of
    T.Void -> return V.Padding
    _ -> pop

-- * Manipulating frame stack

{- |
You should use 'begin_call' instead of using 'enter' directly.

This also inserts the necessary paddings so that category-2 (8-byte) values
take two local variable array indexes.
-}
begin_call :: (Stateful m) => Class -> Method -> [V.Value] -> m ()
begin_call cls met args = enter (f_new cls met (workaround args))
    where
        workaround list = do
            v <- list
            if V.is_category_2 v
                then [v, V.Padding]
                else [v]

get_frame :: (Stateful m) => m Frame
get_frame = do
    fs <- gets s_frames
    case fs of
        [] -> stop Frame_stack_underflow
        f : _ -> return f

replace_frame :: (Stateful m) => Frame -> m ()
replace_frame new_frame = do
    frames <- gets s_frames
    case frames of
        [] -> stop Frame_stack_underflow
        _ : x -> modify (\ s -> s { s_frames = new_frame : x })

modify_frame :: (Stateful m) => (Frame -> Frame) -> m ()
modify_frame fun = do
    fs <- gets s_frames
    case fs of
        [] -> stop Frame_stack_underflow
        f : g -> modify (\ s -> s { s_frames = fun f : g })

{- |
Push the frame to the frame stack.
-}
enter :: (Stateful m) => Frame -> m ()
enter new_frame = do
    debug_call ("[debug] [Jvm_arch] [enter] " ++ cname ++ " " ++ mname ++ " " ++ show (m_signature met))
    modify (\ s -> s { s_frames = new_frame : s_frames s })
    where
        cls = f_class new_frame
        met = f_method new_frame
        cname = Bu.toString $ c_name cls
        mname = Bu.toString $ m_name met

{- |
If the stack is not empty, this removes
the top frame from the frame stack,
and replaces the current frame with that.

If the stack is empty, this stops the machine.
-}
leave :: (Stateful m) => m ()
leave = do
    s <- get
    case s_frames s of
        _ : x -> put s { s_frames = x }
        [] -> stop Frame_stack_underflow

-- * Manipulating local variable array

{- |
Load from local variable array.

Get the element at the given index from
the local variable array of the current frame.
-}
load :: (Stateful m) => Local_index -> m V.Value
load i = do
    frame <- get_frame
    let local = f_local frame
    maybe (stop Invalid_local_index) return (local `at` i)

{- |
Store to local variable array.

Set the element at the given index in the local variable array
of the current frame.
-}
store :: (Stateful m) => Local_index -> V.Value -> m ()
store i v = modify_frame $ \ frame ->
    let
        local = f_local frame
    in
        frame { f_local = Li.replace V.Padding i v local }

-- | Local variable array index begins from 0.
type Local_index = Int

-- * Testing access flags

class Has_access a where get_access :: a -> Word16
instance Has_access Word16 where get_access = id
instance Has_access Method where get_access = m_access

is_static :: (Has_access a) => a -> Bool
is_static a = get_access a .&. 0x0008 /= 0

is_public :: (Has_access a) => a -> Bool
is_public a = get_access a .&. 0x0001 /= 0

is_native :: (Has_access a) => a -> Bool
is_native a = get_access a .&. 0x0100 /= 0

-- * Getting fields and methods

find_method :: (Stateful m) => Class -> Me.Method_name -> T.Signature -> m (Maybe Method)
find_method clas mname msig =
    case [ m | m <- c_methods clas, m_name m == mname, m_signature m == msig ] of
        x : _ -> return (Just x)
        _ -> return Nothing

get_method :: (Stateful m) => Class -> Me.Method_name -> T.Signature -> m Method
get_method clas mname msig = do
    mm <- find_method clas mname msig
    case mm of
        Just x -> return x
        _ -> stop (Method_not_found (c_name clas) mname msig)

get_field :: (Stateful m) => Class -> Me.Field_name -> T.Type -> m Field
get_field clas fname ftype =
    case [ f | f <- c_fields clas, f_name f == fname, f_type f == ftype ] of
        x : _ -> return x
        _ -> stop (Field_not_found (c_name clas) (Me.Mk_field_ref fname ftype))

-- * Manipulating field values

-- Grossly inefficient.
read_static_field :: (Stateful m) => Class -> Me.Field_ref -> m V.Value
read_static_field clas_ fref =
    -- TODO check if field exists?
    return $ Li.kv_get_or (U.def_value (Me.fr_type fref)) fref (c_static clas_)

write_static_field :: (Stateful m) => Class -> Me.Field_ref -> V.Value -> m ()
write_static_field clas_ fref value =
    modify $ \ s ->
        s { s_classes = map set (s_classes s) }
    where
        set c | c_name c == c_name clas_ = c { c_static = Li.kv_upsert fref value (c_static c) }
        set c = c

read_instance_field :: (Stateful m) => V.Value -> Me.Field_ref -> m V.Value
read_instance_field ins fref = case ins of
    V.Instance _ r ->
        maybe (U.def_value ftype) snd . DL.find (\ (f, _) -> f == fref) <$> read_ioref r
    V.Null -> stop Unexpected_null
        -- TODO throw NullPointerException instead; also do the same for write_instance_field
    _ ->
        stop Expecting_instance
    where
        ftype = Me.fr_type fref

write_instance_field :: (Stateful m) => V.Value -> Me.Field_ref -> V.Value -> m ()
write_instance_field target fref value =
    case target of
        V.Instance _ r -> modify_ioref r loop
        V.Null -> stop Unexpected_null
        _ -> stop Expecting_instance
    where
        loop [] = [(fref, value)]
        loop ((fr, _oldval) : rest) | fr == fref = (fr, value) : rest
        loop (_ : rest) = loop rest

-- * Internals: Low-level State and Frame functions

-- | Create a state.
s_new :: State
s_new = Mk_state Ready [] [] ["."] [] False False

-- | Create a frame.
f_new :: Class -> Method -> [V.Value] -> Frame
f_new cls met loc = Mk_frame cls met 0 [] loc

class Is_ready a where is_ready :: a -> Bool

instance Is_ready State where
    is_ready = is_ready . s_status

instance Is_ready Status where
    is_ready Ready = True
    is_ready _ = False

data State
    = Mk_state
    {
        s_status :: Status
        , s_frames :: [Frame] -- ^ frame stack
        , s_classes :: [Class] -- ^ classes that have been loaded (but not necessarily initialized yet)
        , s_classpath :: [FilePath] -- ^ list of directories that will be searched for classes
        , s_native_bindings :: [Binding] -- ^ native methods lacking bodies will be bound to these
        , s_debug_load :: Bool -- ^ whether the class-loading subsystem shall emit debugging information
        , s_debug_call :: Bool -- ^ whether to print debug message on every method call
    }

data Status
    = Ready
    | End_of_program
    | Frame_stack_underflow
    | Operand_stack_underflow
    | Invalid_constant_pool_index
    | Invalid_local_index
    | Method_lacks_Code
    | Method_body_missing
    | Method_body_not_fetchable
    | Invalid_pc Pc
    | Decode_error Word8
    | Need_io -- ^ usually requested by a native method
    | Type_must_be T.Type T.Type -- ^ actual-type expected-type
    | Expecting_fieldref
    | Expecting_methodref
    | Expecting_class_name
    | Expecting_string
    | Expecting_instance
    | Expecting_array
    | Hierarchy_too_deep Int
    | Array_index_out_of_bounds
    | Unexpected_null
    | Failed_loading_class Class_name Message
    | Class_not_found Class_name
    | Field_not_found Class_name Me.Field_ref
    | Field_not_initialized Class_name Me.Field_ref
    | Method_not_found Class_name Me.Method_name T.Signature
    | Negative_array_size
    | Invalid_newarray_type Word8
    | Invalid_tableswitch Message
    | Invalid_lookupswitch Message
    | Invalid_reserved Int Word8
    | Unhandled_exception Message
    | Not_implemented Message
    deriving (Read, Show, Eq)

type Message = String

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
        , f_stack :: [V.Value] -- ^ operand stack
        , f_local :: [V.Value] -- ^ local variable array
    }

{- |
Program counter type.

'Pc' must be unsigned,
'Pc_offset' must be signed,
and they must have the same width.
-}
type Pc = Word32

type Pc_offset = Int32

-- * Native method binding

find_binding :: (Stateful m) => Class_name -> Me.Method_name -> T.Signature -> m (Maybe Method)
find_binding cname mname msig = do
    bins <- gets s_native_bindings
    let matches = [ m | Mk_binding c m <- bins, c == cname, m_name m == mname, m_signature m == msig ]
    return $ case matches of
        [] -> Nothing
        x : _ -> Just x

data Binding
    = Mk_binding
    {
        b_class_name :: Class_name
        , b_method :: Method
    }

-- FIXME handle static/instance?
bind :: (Stateful m) => Class_name -> T.Type -> Me.Method_name -> [T.Type] -> J V.Value -> m ()
bind cname rtype mname atypes body = modify $ \ s ->
    s
        {
            s_native_bindings = Mk_binding cname method : s_native_bindings s
        }
    where
        method = Mk_method 0 mname (T.Mk_signature atypes rtype) (Native_io body)

-- * Types suitable for execution

-- | Resolved class.
data Class
    = Mk_class
    {
        c_name :: Bs.ByteString
        , c_super :: Maybe Bs.ByteString -- ^ binary name of superclass
        , c_fields :: [Field]
        , c_methods :: [Method]
        , c_pool :: [Constant]
        , c_static :: [(Me.Field_ref, V.Value)] -- ^ static field values
        , c_initialized :: Bool
    }

{- |
An inhabitant of this type is an entry in a constant pool that has been resolved.
-}
data Constant
    = C_integer Int32 -- ^ 3
    | C_float Float -- ^ 4
    | C_long Int64 -- ^ 5
    | C_double Double -- ^ 6
    | C_class Bs.ByteString -- ^ 7
    | C_string Bs.ByteString -- ^ 8
    | C_fieldref Class_name Me.Field_name T.Type -- ^ 9
    | C_methodref Class_name Me.Method_name T.Signature -- ^ 10
    | C_resolved
    deriving (Read, Show, Eq)

{- |
Use @toString@ and @fromString@ from "Data.ByteString.UTF8"
to convert 'Class_name' to 'String' or the other way around.
-}
type Class_name = Bs.ByteString

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
        , f_name :: Me.Field_name
        , f_type :: T.Type
    }
    deriving (Read, Show, Eq)

data Method
    = Mk_method
    {
        m_access :: Word16
        , m_name :: Me.Method_name
        , m_signature :: T.Signature
        , m_body :: Body
    }

data Body
    = Missing -- ^ no body
    | Bytecode K.Code -- ^ JVM bytecode
    | Native (S V.Value) -- ^ native without 'IO'
    | Native_io (J V.Value) -- ^ native with 'IO'

-- Instances involving S

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
    fail = stop . Not_implemented
    return x = Mk_s $ \ s -> (s, Just x)
    (>>=) w k = Mk_s $ \ s0 ->
        let
            (s1, ma) = un_s w s0
            (s2, mb) = maybe (s1, Nothing) (\ a -> un_s (k a) s1) ma
        in
            (s2, mb)

instance Stateful S where

    get = Mk_s $ \ s -> (s, Just s)

    put s = Mk_s $ \ _ -> (s, Just ())

    stop reason = Mk_s (\ s -> (s { s_status = reason }, Nothing))

-- Instances involving J

instance Functor J where
    fmap f m = Mk_j $ \ s ->
        fmap
            (\ (t, e) -> (t, fmap f e))
            (un_j m s)

instance Applicative J where
    pure = return
    (<*>) = M.ap

instance Monad J where
    fail = stop . Not_implemented
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

instance Stateful J where
    get = lift get
    put = lift . put
    stop = lift . stop
    debug_call msg = do
        b <- gets s_debug_call
        M.when b $ liftIO $ putStrLn msg
    read_ioref = liftIO . Ir.readIORef
    modify_ioref r f = liftIO $ Ir.modifyIORef' r f

instance MonadIO J where
    liftIO = j_lift_io
