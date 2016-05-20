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

import qualified Jvm_io as Z
import qualified Jvm_state as S

import Jvm_state
    (
        Class(..)
        , Frame(..)
        , Instruction(..)
        , State(..)
        , Reason(..)
        , Value(..)
        , f_new
        , s_new
        , s_code
    )

-- * Bytecode execution or interpretation

step :: J ()
step = do
    ins <- decode
    -- XXX should use vector (or Map Int) instead of list
    case ins of
        Nop -> return ()
        -- Lload i -> get_local i >>= push
        Getstatic c -> error "NOT IMPLEMENTED GETSTATIC"
            -- get_const c >>= push -- FIXME typecheck
        Iconst_m1 -> iconst (negate 1)
        Iconst_0 -> iconst 0
        Iconst_1 -> iconst 1
        Iconst_2 -> iconst 2
        Iconst_3 -> iconst 3
        Iconst_4 -> iconst 4
        Iconst_5 -> iconst 5
        If_icmpge ofs -> do
            -- XXX
            x <- pop
            y <- pop
            -- let success = x >= y
            error "NOT IMPLEMENTED" -- MUST READ INSTRUCTION STREAM AS BYTE ARRAY
        -- Iload im -> get_local im >>= push
        -- Istore im -> pop >>= istore im
        Return -> leave
        _ -> stop $ Unknown_instruction $ show ins
    where
        iconst :: Int32 -> J ()
        iconst v = push (Integer v)

        istore :: Int -> Value -> J ()
        istore i v = modify $ \ s ->
            let
                frame = s_frame s
                local = f_local frame
                filler = Integer 0
            in
                s { s_frame = frame { f_local = replace filler i v local } }
        replace :: a -> Int -> a -> [a] -> [a]
        replace filler index x [] | index <= 0 = [x]
        replace filler index x (_ : z) | index <= 0 = x : z
        replace filler index x [] = filler : replace filler (index - 1) x []
        replace filler index x (y : z) = y : replace filler (index - 1) x z

run :: J ()
run = step >> run

-- * Virtual machine primitives

fetch :: J Word8
fetch = lift S.fetch

{- |
Fetch and decode the next instruction, and increment pc accordingly.
-}
decode :: J Instruction
decode = lift S.decode

{- |
Remove the top frame from the frame stack,
and replace the current frame with that.
(Pop the frame stack.)

If the stack is empty, this stops the machine.
-}
leave :: J ()
leave = modify S.leave

-- | Pop value from operand stack.
pop :: J Value
pop = Mk_j $ return . S.pop

-- | Push value to operand stack.
push :: Value -> J ()
push = modify . S.push

stop :: Reason -> J a
stop r = modify (S.stop_raw r) >> Mk_j (\ s -> return (s, Nothing))

lift :: S.S a -> J a
lift x = Mk_j $ return . S.un_s x

get_local :: Int -> J Value
get_local i = do
    s <- get_state
    case f_local (s_frame s) `Z.at` i of
        Nothing -> stop Invalid_local_index
        Just v -> return v

-- * J Monad: instruction decoding etc

newtype J a = Mk_j { un_j :: State -> IO (State, Maybe a) }

get_state :: J State
get_state = Mk_j $ \ s -> return (s, Just s)

put_state :: State -> J ()
put_state s = Mk_j $ \ _ -> return (s, Just ())

exec :: J a -> State -> IO State
exec comp init_state = fst <$> un_j comp init_state

modify :: (State -> State) -> J ()
modify f = Mk_j $ \ s -> return (f s, Just ())

instance Functor J where
    fmap f m = Mk_j $ \ s ->
        fmap
            (\ (s, e) -> (s, fmap f e))
            (un_j m s)

instance Applicative J where
    pure = return
    (<*>) = M.ap

instance Monad J where
    return a = Mk_j $ \ s -> return (s, Just a)
    (>>=) m k = Mk_j $ \ s_0 -> do
        case s_running s_0 of
            False ->
                return (s_0, Nothing)
            True -> do
                (s_1, ma) <- un_j m s_0
                case s_running s_1 of
                    False -> return (s_1, Nothing)
                    True -> case ma of
                        Nothing -> return (s_1, Nothing)
                        Just a -> un_j (k a) s_1

-- * Testing

testload :: IO ()
testload = do
    let clspath = "Hello.class"
    Right cls <- Z.load_class_file clspath
    print cls
    Right rcl <- return $ S.resolve_class cls
    print rcl
    M.forM_ (c_methods rcl) $ \ m -> do
        let (s, i) = S.disassemble rcl m
        print $ s_stop_reason s
        print i

jvm :: IO ()
jvm = do
    let clspath = "Hello.class"
    Right cls <- S.load_class_file clspath
    main_method <- case filter S.is_main (c_methods cls) of
        [] -> io_error "has no static main (String[]) method"
        [x] -> return x
        x -> io_error $ "too many main methods: " ++ show x
    let init_state = s_new (f_new cls main_method)
    final_state <- exec run init_state
    putStrLn $ pretty final_state
    return ()
    where
        io_error :: String -> IO a
        io_error = Ie.ioError . Ie.userError
        pretty :: State -> String
        pretty state =
            unlines $
                [
                    "running = " ++ show (s_running state)
                    , "stop_reason = " ++ show (s_stop_reason state)
                    , "pc = " ++ show (f_pc frame)
                    , "method = " ++ show method
                ]
            where
                frame = s_frame state
                method = f_method frame
                pc = f_pc frame
