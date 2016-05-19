{- |
This module contains a Java bytecode interpreter.
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

import qualified Control.Monad as M
import qualified System.IO.Error as Ie

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Unsafe as Bsu

import qualified Data.Serialize as Se

import qualified Data.ByteString.UTF8 as Bu

import qualified Jvm_arch as A
import qualified Jvm_decode as D
import qualified Jvm_io as Z
import qualified Jvm_prepare as P
import qualified Jvm_state as S

import Jvm_arch
    (
        J(..)
        , S(..)
        , State
        , Status
        , Instruction
        , is_ready
        , lift
        , Class(..)
        , Value(..)
    )

-- * Architecture

{- $
"Jvm_io": parse a 'Bs.ByteString' as found on disk, memory, network.

Some types have the same names.

There are two 'Monad's: S and J.

J is an extension of S that allows 'IO'.

Bytecode execution happens mostly in S.
-}

-- * Calling Java code from Haskell

-- * Writing Java @native@ methods in Haskell

-- * Implementing Java interface in Haskell

-- * Calling Haskell code from Java

-- * Bytecode execution or interpretation

step :: J ()
step = lift S.step

run :: J ()
run = step >> run

-- * Virtual machine primitives

{- |
Fetch and decode the next instruction, and increment pc accordingly.
-}
decode :: J Instruction
decode = lift D.decode

{- |
Remove the top frame from the frame stack,
and replace the current frame with that.
(Pop the frame stack.)

If the stack is empty, this stops the machine.
-}
leave :: J ()
leave = lift S.leave

-- | Pop value from operand stack.
pop :: J Value
pop = lift S.pop

-- | Push value to operand stack.
push :: Value -> J ()
push = lift . S.push

stop :: Status -> J a
stop = lift . A.stop

load :: Int -> J Value
load = lift . S.load

store :: Int -> Value -> J ()
store n v = lift (S.store n v)

-- * Testing

testload :: IO ()
testload = do
    let clspath = "Hello.class"
    Right cls <- Z.load_class_file clspath
    Right rcl <- return $ P.resolve_class cls
    M.forM_ (c_methods rcl) $ \ m -> do
        let (s, i) = S.disassemble rcl m
        print $ A.s_status s
        print i

jvm :: IO ()
jvm = do
    let clspath = "Hello.class"
    e_jls <- S.load_class_file "jre/lib/rt/java/lang/System.class"
    let
        -- Left er = e_jls
        Right jls = e_jls
    -- print er
    Right cls <- S.load_class_file clspath
    let [entry_point] = [ m | m <- c_methods cls, S.name_is "test" m ]
    {-
    entry_point <- case filter S.is_main (c_methods cls) of
        [] -> io_error "has no static main (String[]) method"
        [x] -> return x
        x -> io_error $ "too many main methods: " ++ show x
    -}
    let init_state = (A.s_new (A.f_new cls entry_point))
            {
                A.s_classes = [jls, cls]
            }
    final_state <- flip A.exec init_state $ do
        store 0 (Integer 5)
        store 1 (Integer 4)
        run
    putStrLn $ S.dump final_state
    return ()
    where
        io_error :: String -> IO a
        io_error = Ie.ioError . Ie.userError
