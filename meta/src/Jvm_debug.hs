{- |
This module provides functionalities for
helping human programmers debug this package.
-}
module Jvm_debug
where

import qualified Data.List as DL

import qualified Data.ByteString.UTF8 as Bu

import Meta.JvmArch
import Meta.JvmIns
    (
        Instruction
    )

import qualified Jvm_decode as D
import qualified Meta.JvmValue as V
import qualified Meta.JvmType as T

-- * Disassembling bytecode

disassemble :: Class -> Method -> S [(Pc, Instruction)]
disassemble cls m = do
    begin_call cls m []
    loop
    where
        with_def :: a -> S a -> S a
        with_def d c = Mk_s $ \ s0 ->
            case un_s c s0 of
                (_, Nothing) -> (s0, Just d)
                (s1, m1) -> (s1, m1)
        loop = with_def [] $ do
            p <- D.get_pc
            i <- D.decode
            rest <- loop
            return $ (p, i) : rest

-- * Dumping state

{- |
Make a human-readable representation of the 'State'.
-}
dump :: State -> String
dump state =
    unlines
        [
            "status:"
            , ""
            , show (s_status state)
            , ""
        ]
    ++
    case s_frames state of
        [] -> ""
        frame : _ ->
            let
                bool c f t = if c then t else f
                clas = f_class frame
                method = f_method frame
                pc = f_pc frame
                signature = m_signature method
                disassembly =
                    case eval (disassemble clas method) s_new of
                        Just ins ->
                            let
                                (numbers, instructions) = (map (show . fst) ins, map (show . snd) ins)
                                left_column_width = maximum (map length numbers)
                                align x = replicate (left_column_width - length x) ' ' ++ x
                            in
                                unlines $ zipWith (\ n i -> align n ++ "    " ++ i) numbers instructions
                        Nothing ->
                            ""
            in
                unlines $
                    [
                        "pc: " ++ show (f_pc frame)
                        , ""
                        , "class " ++ Bu.toString (c_name clas)
                        , ""
                        , ""
                            ++ bool (is_public method) "" "public "
                            ++ bool (is_static method) "" "static "
                            ++ bool (is_native method) "" "native "
                            ++ T.pretty (T.s_return_type signature)
                            ++ " " ++ Bu.toString (m_name method)
                            ++ " (" ++ DL.intercalate ", " (map T.pretty $ T.s_arg_types signature) ++ ")"
                        , ""
                        , disassembly
                        , ""
                        , "operand stack (top line is top of stack):"
                        , ""
                        , unlines (map V.pretty $ f_stack frame)
                    ]
