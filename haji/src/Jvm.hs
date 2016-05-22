{- |
This module contains a Java bytecode interpreter.

This module is the public interface of the package.
Every other module in the package is a private internal module
that is included here only for documentation completeness.
You should only use this public module
and not directly use those private modules.
-}
module Jvm
where

import Data.Bits
    (
        (.&.)
    )
import Data.Int
    (
        Int16
        , Int32
    )
import Data.Word
    (
        Word8
        , Word16
        , Word32
    )
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Control.Monad as M
import qualified System.IO.Error as Ie

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Unsafe as Bsu

import qualified Data.Serialize as Se

import qualified Data.ByteString.UTF8 as Bu

import Jvm_arch
    (
        J(..)
        , S(..)
        , State
        , Status
        , is_ready
        , lift
        , Class(..)
    )
import Jvm_instruction
    (
        Instruction
    )

import qualified Jvm_arch as A
import qualified Jvm_build as B
import qualified Jvm_debug as C
import qualified Jvm_decode as D
import qualified Jvm_execute as E
import qualified Jvm_interop as O
import qualified Jvm_load as L
import qualified Jvm_prepare as P
import qualified Jvm_state as S
import qualified Jvm_type as T
import qualified Jvm_value as V

-- * Architecture

{- $
"Jvm_io": parse a 'Bs.ByteString' as found on disk, memory, network.

Some types have the same names.

There are two 'Monad's: S and J.

J is an extension of S that allows 'IO'.

Bytecode execution happens mostly in S.
-}

-- * The default way of running a virtual machine

{- |
Create a virtual machine with the given configuration.

This returns the final state of the virtual machine.
-}
runvm :: Option -> IO A.State
runvm option = do
    flip A.exec_io init_state $ do
        -- TODO implement Nothing and Just From_jar
        let Just (From_class_path main_class_name) = o_main_class option
        main_class <- E.load_and_init_class (Bu.fromString main_class_name)
        O.call main_class_name
            -- DEBUG T.Void "main" [T.Array (T.Instance (Bu.fromString "java/lang/String"))]
            T.Void "main" []
            []
    where
        main_class_name = o_main_class option
        init_state = A.s_new
            {
                A.s_classpath = o_classpath option
                , A.s_native_bindings =
                    [
                        -- XXX
                        A.Mk_binding (Bu.fromString "java/lang/Class") $
                            A.Mk_method 0 (Bu.fromString "registerNatives") (T.Mk_signature [] T.Void) $ A.Native_io $ do
                                return V.Padding
                        ,
                        A.Mk_binding (Bu.fromString "java/lang/Object") $
                            A.Mk_method 0 (Bu.fromString "registerNatives") (T.Mk_signature [] T.Void) $ A.Native_io $ do
                                return V.Padding
                        ,
                        -- FIXME
                        A.Mk_binding (Bu.fromString "java/lang/System") $
                            A.Mk_method 0 (Bu.fromString "identityHashCode") (T.Mk_signature [T.Instance (Bu.fromString "java/lang/Object")] T.Int) $ A.Native_io $ do
                                return $ V.Integer 0
                        ,
                        A.Mk_binding (Bu.fromString "java/lang/System") $
                            A.Mk_method 0 (Bu.fromString "registerNatives") (T.Mk_signature [] T.Void) $ A.Native_io $ do
                                return V.Padding
                        ,
                        -- FIXME
                        A.Mk_binding (Bu.fromString "java/lang/System") $
                            A.Mk_method 0 (Bu.fromString "nanoTime") (T.Mk_signature [] T.Long) $ A.Native_io $ do
                                return (V.Long 0)
                        ,
                        A.Mk_binding (Bu.fromString "java/lang/System") $
                            A.Mk_method 0 (Bu.fromString "currentTimeMillis") (T.Mk_signature [] T.Long) $ A.Native_io $ do
                                return (V.Long 0)
                        ,
                        A.Mk_binding (Bu.fromString "Hello") $
                            A.Mk_method 0 (Bu.fromString "println") (T.Mk_signature [T.Int] T.Void) $ A.Native_io $ do
                                V.Integer x <- A.load 0 -- XXX
                                liftIO . putStrLn $ show x
                                return V.Padding
                        ,
                        A.Mk_binding (Bu.fromString "Hello") $
                            A.Mk_method 0 (Bu.fromString "println") (T.Mk_signature [T.Long] T.Void) $ A.Native_io $ do
                                V.Long x <- A.load 0 -- XXX
                                liftIO . putStrLn $ show x
                                return V.Padding
                    ]
                -- DEBUG
                , A.s_debug_load = True
                , A.s_debug_call = True
            }
        boot_class = B.class_ "<bootstrap>" $ do
            B.native B.Public T.Void "<main>" [] $ return V.Padding
        boot_method =
            case c_methods boot_class of
                [m] -> m
                _ -> error $ "Jvm.runvm.boot_method: assertion error: "
                    ++ "bootstrapper wants exactly one method, "
                    ++ "and the method has to be named @<main>@"
        frame = A.f_new boot_class boot_method []

testrunvm :: IO ()
testrunvm = do
    s <- runvm option
    putStrLn $ C.dump s
    where
        option = option_def
            {
                o_main_class = Just (From_class_path "Hello")
                , o_classpath = [".", "jre/lib/rt"] -- XXX
            }

type Class_name = String

-- * Command-line options

{- |
An inhabitant of this type is a configuration that
can be manipulated by command-line options
to change the behavior of the virtual machine.

Use 'option_def' to make an 'Option'.
-}
data Option
    = Mk_option
    {
        o_verbose :: Bool
        , o_main_class :: Maybe Main_class
        , o_classpath :: [FilePath]
    }
    deriving (Read, Show)

{- |
An inhabitant of this type tells the virtual machine
about how to find the main class.
-}
data Main_class
    = No_main_class
    | From_jar FilePath
    | From_class_path Class_name
    deriving (Read, Show)

-- | This is the default virtual machine configuration.
option_def :: Option
option_def = Mk_option
    {
        o_verbose = False
        , o_main_class = Nothing
        , o_classpath = ["."]
    }

-- * Utility: listing class members

list :: String -> IO ()
list cname = do
    flip A.exec_io state $ do
        c <- L.load_class (Bu.fromString cname)
        A.j_lift_io $ putStrLn $ flip concatMap (A.c_methods c) $ \ m ->
            Bu.toString (A.m_name m) ++ " " ++ show (A.m_signature m) ++ "\n"
        return ()
    return ()
    where
        state = A.s_new
        bo = B.bootstrap what
        what :: S V.Value
        what = return V.Null

-- * Testing

jvm :: IO ()
jvm = do
    let clspath = "Hello.class"
    let
        bo = B.bootstrap what
        what :: S V.Value
        what = do
            return V.Null
    -- print er
    Right cls <- L.load_class_file clspath
    let [entry_point] = [ m | m <- c_methods bo, A.m_name m == Bu.fromString "<main>" ]
    -- let [entry_point] = [ m | m <- c_methods cls, S.name_is "test" m ]
    {-
    entry_point <- case filter S.is_main (c_methods cls) of
        [] -> io_error "has no static main (String[]) method"
        [x] -> return x
        x -> io_error $ "too many main methods: " ++ show x
    -}
    let init_state = A.s_new
            {
                A.s_classpath = [".", "jre/lib/rt"]
                , A.s_debug_load = True
            }
    final_state <- flip A.exec_io init_state $ do
        -- O.call "Hello" T.Int "test2" [T.Int, T.Int] [V.Integer 200, V.Integer 100] >>= A.push
        O.call "Hello" T.Int "virtual" [T.Int] [V.Null, V.Integer 12345] >>= A.push
    putStrLn $ C.dump final_state
    return ()
    where
        io_error :: String -> IO a
        io_error = Ie.ioError . Ie.userError
