module Meta.UserCmd (
    command_line
    , command_line_
) where

import qualified Control.Monad as Monad
import qualified System.Environment as Env

import qualified System.Directory as Dir

import qualified Meta.JavaWebApp as JWA
import qualified Meta.UserGenJava as UGJ

{- |
Command line for controlling the application and related common tasks.
-}
command_line :: JWA.App -> IO ()
command_line app = Env.getArgs >>= command_line_ app

-- | This is 'command_line' but you pass the command-line arguments yourself.
command_line_ :: JWA.App -> [String] -> IO ()
command_line_ app = parse Monad.>=> run
    where
        parse :: (Monad m) => [String] -> m Command
        parse args = case args of
            [] -> pure Nop
            "cd" : path : rest -> Seq (Chdir path) <$> parse rest
            "generate" : rest -> Seq (Generate app) <$> parse rest
            "help" : rest -> Seq Help <$> parse rest
            "recompile" : rest -> Seq (Recompile app) <$> parse rest
            _ -> fail $ "Invalid arguments: " ++ show args
        run :: Command -> IO ()
        run cmd = case cmd of
            Nop -> return ()
            Help -> do
                prog <- Env.getProgName
                putStr $
                    "Usage: " ++ prog ++ " COMMAND...\n"
                    ++ "A COMMAND is any of these:\n"
                    ++ "    cd DIR      change directory to DIR\n"
                    ++ "    help        show this\n"
                    ++ "    generate    generate Java source code\n"
                    ++ "    recompile   recompile generated Java source code\n"
            Chdir path -> Dir.setCurrentDirectory path
            Seq a b -> run a >> run b
            Generate ap -> UGJ.generate_java ap
            Recompile ap -> UGJ.maven_recompile ap

data Command
    = Nop
    | Seq Command Command
    | Chdir FilePath
    | Generate JWA.App
    | Help
    | Recompile JWA.App
    deriving (Read, Show)
