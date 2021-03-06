{- |
This module contains a Java bytecode interpreter.

This module is the public interface of the package.
Every other module in the package is a private internal module
that is included here only for documentation completeness.
You should only use this public module
and not directly use those private modules.

* How do I decode bytecode? Where is @decode : [Word8] -> [Instruction]@?

* How do I optimize bytecode?

    * How do I translate JVM bytecode into "Meta.Stk" abstract stack machine instruction?

        * Then how do I translate that to lambda calculus?

            * Translate to lambda calculus using lots of @let@s?

            * Or translate to CPS (contiuation-passing style) first instead?

* How do I find every class that has @something@ in its constant pool?

* How do I parse a class file?

    * What packages might be related?

        * jvm-binary: A library for reading Java class-files
        <https://hackage.haskell.org/package/jvm-binary>

            * That package uses type families and Template Haskell.

        * jvm-parser: A parser for JVM bytecode files
        <https://hackage.haskell.org/package/jvm-parser>

        * dbjava: Decompiler Bytecode Java
        <https://hackage.haskell.org/package/dbjava>

        * hs-java: Java .class files assembler/disassembler
        <https://hackage.haskell.org/package/hs-java>

        * javaclass: Java class files
        <https://hackage.haskell.org/package/javaclass>

        * language-java-classfile: Parser for Java .class files
        <https://hackage.haskell.org/package/language-java-classfile>

        * javav: A utility to print the target version of Java class files.
        <https://hackage.haskell.org/package/javav>

* Related packages

    * JVM

        * <https://hackage.haskell.org/packages/search?terms=jvm>

        * jvm: Call any JVM function from Haskell.
        <https://hackage.haskell.org/package/jvm>

        * HJVM: A library to create a Java Virtual Machine and manipulate Java objects
        <https://hackage.haskell.org/package/HJVM>

        * inline-java: Java interop via inline Java code in Haskell modules.
        <https://hackage.haskell.org/package/inline-java>

        * java-bridge: Bindings to the JNI and a high level interface generator.
        <https://hackage.haskell.org/package/java-bridge>

        * jarfind: Tool for searching java classes, members and fields in classfiles and JAR archives
        <https://hackage.haskell.org/package/jarfind>

        * java-adt: Create immutable algebraic data structures for Java.
        <https://hackage.haskell.org/package/java-adt>

        * jdi: Implementation of Java Debug Interface
        <https://hackage.haskell.org/package/jdi>

    * Java

        * java-character: Functions to simulate Java's Character class.
        <https://hackage.haskell.org/package/java-character>

        * cantor: Application for analysis of java source code
        <https://hackage.haskell.org/package/cantor>

    * Non-JVM

        * simple-stacked-vm: Simple stacked virtual machine: assembler, disassembler, bytecode interpreter
        <https://hackage.haskell.org/package/simple-stacked-vm>
-}
module Jvm (
    -- * Class
    C.Class_file
    , Class_for_translation
    , Class_for_interpretation
    -- * Parsing class files
    -- ** Method 1
    , S.parse_class
    -- ** Method 2
    , S.parse_class_0
    , P.Class
    , P.Entry
    , P.Raw
    , P.Resolved
    -- * Accessing the constant pool
    , P.get_constant_pool
    -- ** Resolving the constant pool
    -- $resolve

    -- *** Method 1: the Jvm_prepare module
    , Prep.resolve_class
    -- *** Method 2: the Meta.JvmConstPool module
    , P.resolve_constant_pool
    , P.Entry_(..)
    , P.Index
    , P.get_name
    -- * Decoding bytecode
    , Dec.decode
    -- ** Disassembling bytecode
    , D.disassemble
    -- ** Dumping state
    , D.dump
    -- * Architecture
    -- $arch
    -- * Running
    , runvm
    , testrunvm
    , Class_name
    , A.State(..)
    -- * Command-line options
    , Option(..)
    , Main_class(..)
    , option_def
    -- * Utility: listing class members
    , list
    -- * Testing
    , jvm
) where

import Prelude ()
import Meta.Prelude

import qualified Control.Monad as M
import qualified Foreign as F
import qualified System.IO.Error as Ie

import qualified Data.ByteString.UTF8 as Bu

import Meta.JvmArch
    (
        S(..)
        , Class(..)
    )
import Meta.JvmMember (fr_type, fr_name)

import qualified Jvm_build as B
import qualified Jvm_decode as Dec
import qualified Jvm_debug as D
import qualified Jvm_execute as E
import qualified Jvm_interop as O
import qualified Jvm_load as L
import qualified Jvm_prepare as Prep
import qualified Meta.JvmArch as A
import qualified Meta.JvmCls as C
import qualified Meta.JvmConstPool as P
import qualified Meta.JvmSer as S
import qualified Meta.JvmType as T
import qualified Meta.JvmTys as U
import qualified Meta.JvmValue as V

-- | The representation of a class for translating bytecode to another form.
type Class_for_translation = A.Class

-- | The representation of a class for execution by bytecode interpretation.
type Class_for_interpretation = A.Class

{- $resolve
A /class/ is several concepts:

    * the representation for storage in a file in a disk

    * the representation for theoretical manipulation or transformation

    * the representation for execution by JVM

There are two choices:

    * Use type parameters and type families.

        * Pro: Better documentation.

        * Con: Type parameters prevent unboxing of the instantiated parameter.

        * Con: Type families prevent the automatic @deriving@ of type class instances.

        * Con: Requires recent GHC.

    * Use two types in different modules.
-}

{- $arch
There are two 'Monad' instances: 'S' and 'J'.

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
        A.bind (Bu.fromString "java/lang/Class")
            T.Bool (Bu.fromString "desiredAssertionStatus0") [T.Instance (Bu.fromString "java/lang/Class")] $ do
                -- return $ V.Bool 0
                return $ V.Bool 1 -- XXX
        A.bind (Bu.fromString "java/lang/Float")
            T.Int (Bu.fromString "floatToRawIntBits") [T.Float] $ do
                V.Float x <- A.load 0 -- FIXME
                fmap V.Integer $ liftIO $ F.alloca $ \ p -> do
                    F.poke p x
                    F.peek (F.castPtr p) -- FIXME
        A.bind (Bu.fromString "java/lang/Double")
            T.Long (Bu.fromString "doubleToRawLongBits") [T.Double] $ do
                V.Double x <- A.load 0 -- FIXME
                fmap V.Long $ liftIO $ F.alloca $ \ p -> do
                    F.poke p x
                    F.peek (F.castPtr p) -- FIXME
        A.bind (Bu.fromString "java/lang/Class")
            (T.Instance (Bu.fromString "java/lang/Class")) (Bu.fromString "getPrimitiveClass") [T.Instance (Bu.fromString "java/lang/String")] $ do
                a0 <- A.load 0
                V.Array _ n r <- O.call "java/lang/String" (T.Array T.Char) "toCharArray" [] [a0]
                input <- V.string_from_jchar_list . take (fromIntegral n) <$> A.read_ioref r
                case input of
                    "int" -> return V.Null -- FIXME
                    "float" -> return V.Null -- FIXME
                    "double" -> return V.Null -- FIXME
                    _ -> A.stop (A.Not_implemented $ "java.lang.Class:getPrimitiveClass: " ++ input)
        A.bind (Bu.fromString "Hello")
            T.Void (Bu.fromString "println") [T._Object] $ do
                obj <- A.load 0 -- FIXME
                case obj of
                    V.Instance t r -> do
                        fieldvals <- A.read_ioref r
                        fieldshows <- liftIO $ mapM (\ (f, v) -> (,) (T.pretty (fr_type f) ++ " " ++ Bu.toString (fr_name f)) <$> V.pretty_io v) fieldvals
                        liftIO $ putStrLn $ show t ++ "\n" ++ unlines (map (\ (f, v) -> f ++ " = " ++ v) fieldshows)
                    _ ->
                        liftIO $ putStrLn $ V.pretty obj
                return V.Padding
        A.bind (Bu.fromString "sun/misc/VM") -- XXX
            T.Void (Bu.fromString "initialize") [] $ do
                return V.Padding
        A.bind (Bu.fromString "java/lang/System")
            T.Void (Bu.fromString "arraycopy") [T._Object, T.Int, T._Object, T.Int, T.Int] $ do
                -- FIXME
                src@(V.Array _ _ rs) <- A.load 0
                V.Integer src_begin_ <- A.load 1
                dst@(V.Array dst_elem_type dst_length_ rd) <- A.load 2
                V.Integer dst_begin_ <- A.load 3
                V.Integer count_ <- A.load 4
                let
                    src_begin = fromIntegral src_begin_
                    dst_begin = fromIntegral dst_begin_
                    dst_length = fromIntegral dst_length_
                    count = fromIntegral count_
                asrc <- A.read_ioref rs
                A.modify_ioref rd $ \ adst ->
                    let
                        xadst = adst ++ repeat (U.def_value dst_elem_type)
                    in
                        take dst_begin xadst
                        ++ take count (drop src_begin (asrc ++ repeat (U.def_value dst_elem_type)))
                        ++ take (dst_length - (dst_begin + count)) (drop (dst_begin + count) xadst)
                return $ V.Bool 0
        -- TODO implement Nothing and Just From_jar
        let Just (From_class_path main_class_name) = o_main_class option
        main_class <- E.load_and_init_class (Bu.fromString main_class_name)
        O.call main_class_name
            -- DEBUG T.Void "main" [T.Array (T.Instance (Bu.fromString "java/lang/String"))]
            T.Void "main" []
            []
    where

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
    putStrLn $ D.dump s
    where
        option = option_def
            {
                o_main_class = Just (From_class_path "Hello")
                , o_classpath = [".", "jre/lib/rt"] -- XXX
            }

type Class_name = String

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

list :: String -> IO ()
list cname = do
    M.void $ flip A.exec_io state $ do
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

jvm :: IO ()
jvm = do
    let clspath = "Hello.class"
    let
        bo = B.bootstrap what
        what :: S V.Value
        what = do
            return V.Null
    -- print er
    cls <- L.load_class_file clspath >>= either fail return
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
    putStrLn $ D.dump final_state
    return ()
    where
        io_error :: String -> IO a
        io_error = Ie.ioError . Ie.userError
