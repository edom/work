module Meta.UserCmd (
    command_line
    , command_line_
) where

import Prelude ()
import Meta.Prelude

import qualified Control.Monad as Monad
import qualified System.Environment as Env

import qualified Meta.JavaWebApp as JWA
import qualified Meta.OsProc as Os
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
            "cd" : path : rest -> Seq (Setcwd path) <$> parse rest
            "generate" : rest -> Seq (Generate app) <$> parse rest
            "help" : rest -> Seq Help <$> parse rest
            "readpg" : rest -> Seq ReadPg <$> parse rest
            "compile" : rest -> Seq (Compile app) <$> parse rest
            "dbuild" : rest -> Seq (Dbuild app) <$> parse rest
            "drun" : rest -> Seq (Drun app) <$> parse rest
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
                    ++ "    dbuild      Build Docker image.\n"
                    ++ "    drun        Run Docker image.\n"
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
            Setcwd path -> Os.setcwd path
            Seq a b -> run a >> run b
            Generate ap -> UGJ.generate_java ap
            ReadPg -> SC.test
            Compile ap -> maven_recompile ap
            Dbuild ap -> docker_build ap
            Drun ap -> docker_run ap

in_app_dir :: JWA.App -> IO a -> IO a
in_app_dir app = Os.withcwd dir
    where
        dir = UGJ.get_pom_xml_dir app

maven_recompile :: JWA.App -> IO ()
maven_recompile app = in_app_dir app $ Os.call "mvn" ["clean", "compile"]

docker_tag :: JWA.App -> String
docker_tag app = JWA.get_artifact_id app

docker_build :: JWA.App -> IO ()
docker_build app = in_app_dir app $ Os.call "docker" ["build", "--tag", docker_tag app, "."]

docker_run :: JWA.App -> IO ()
docker_run app = in_app_dir app $ Os.call "docker" ["run", "--rm", "--interactive", "--tty", docker_tag app]

data Command
    = Nop
    | Seq Command Command
    | Setcwd FilePath
    | Generate JWA.App
    | Help
    | ReadPg
    | Compile JWA.App
    | Dbuild JWA.App
    | Drun JWA.App
    deriving (Read, Show)
