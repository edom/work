module Meta.UserCmd (
    command_line
    , command_line_
) where

import Prelude ()
import Meta.Prelude

import qualified Control.Monad as Monad
import qualified System.Environment as Env

import qualified System.Directory as Dir

import qualified Meta.JavaWebApp as JWA
import qualified Meta.SqlCon as SC
import qualified Meta.UserGenJava as UGJ

{- |
Command line for controlling the application and related common tasks.
-}
command_line :: JWA.App -> IO ()
command_line app = Env.getArgs >>= command_line_ app

-- | This is 'command_line' but you pass the command-line arguments yourself.
command_line_ :: JWA.App -> [String] -> IO ()
command_line_ app = friendly_parse Monad.>=> run
    where
        friendly_parse [] = return Help
        friendly_parse rest = parse rest
        parse :: (Functor m, Monad m) => [String] -> m Command
        parse args = case args of
            [] -> return Nop
            "cd" : path : rest -> Seq (Chdir path) <$> parse rest
            "generate" : rest -> Seq (Generate app) <$> parse rest
            "help" : rest -> Seq Help <$> parse rest
            "readpg" : rest -> Seq ReadPg <$> parse rest
            "compile" : rest -> Seq (Compile app) <$> parse rest
            _ -> fail $ "Invalid arguments. Try \"help\" without quotes. The invalid arguments are " ++ show args ++ "."
        run :: Command -> IO ()
        run cmd = case cmd of
            Nop -> return ()
            Help -> do
                prog <- Env.getProgName
                putStr $
                    "Usage: " ++ prog ++ " Command...\n"
                    ++ "\n"
                    ++ "A Command is any of these:\n"
                    ++ "\n"
                    ++ "    help        Show this.\n"
                    ++ "\n"
                    ++ "    cd DIR      Change directory to DIR.\n"
                    ++ "\n"
                    ++ "    generate    Generate Java source code, SQL DDL file, and Dockerfile.\n"
                    ++ "    compile     Compile generated Java source code.\n"
                    ++ "\n"
                    ++ "    readpg      Read PostgreSQL database.\n"
                    ++ "                Destination is read from libpq environment variables PGHOST, PGDATABASE, PGUSER.\n"
                    ++ "                Password is read from PGPASSFILE, which defaults to $HOME/.pgpass.\n"
                    ++ "                See <https://www.postgresql.org/docs/current/static/libpq-envars.html>.\n"
                    ++ "\n"
                    ++ "A Command may span several command-line arguments.\n"
                    ++ "\n"
                    ++ "Example: The invocation \"" ++ prog ++ " cd foo generate compile\" changes directory to \"foo\",\n"
                    ++ "generates the source code, and then compiles it."
                    ++ "\n"
            Chdir path -> Dir.setCurrentDirectory path
            Seq a b -> run a >> run b
            Generate ap -> UGJ.generate_java ap
            ReadPg -> SC.test
            Compile ap -> UGJ.maven_recompile ap

data Command
    = Nop
    | Seq Command Command
    | Chdir FilePath
    | Generate JWA.App
    | Help
    | ReadPg
    | Compile JWA.App
    deriving (Read, Show)
