{-# LANGUAGE RecordWildCards #-}

module Meta.UserCmd (
    command_line
    , command_line_
) where

import Prelude ()
import Meta.Prelude

import qualified Control.Monad as Monad

import qualified Meta.Docker as Doc
import qualified Meta.Http as Http
import qualified Meta.JavaWebApp as JWA
import qualified Meta.Json as Json
import qualified Meta.Os as Os
import qualified Meta.Pretty as P
import qualified Meta.SqlCon as SC
import qualified Meta.UserGenJava as UGJ

{- |
Command line for controlling the application and related common tasks.
-}
command_line :: JWA.App -> IO ()
command_line app = Os.getArgs >>= command_line_ app

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
            "clean" : rest -> Seq Clean <$> parse rest
            "generate" : rest -> Seq Generate <$> parse rest
            "help" : rest -> Seq Help <$> parse rest
            "readpg" : rest -> Seq ReadPg <$> parse rest
            "compile" : rest -> Seq Compile <$> parse rest
            "package" : rest -> Seq Package <$> parse rest
            "dbuild" : rest -> Seq Dbuild <$> parse rest
            "drun" : rest -> Seq Drun <$> parse rest
            "dpush" : rest -> Seq Dpush <$> parse rest
            "ksvc" : rest -> Seq Ksvc <$> parse rest
            "ksvcs" : rest -> Seq Ksvcs <$> parse rest
            _ -> fail $ "Invalid arguments. Try \"help\" without quotes. The invalid arguments are " ++ show args ++ "."

        run :: Command -> IO ()
        run cmd = case cmd of
            Nop -> return ()
            Help -> do
                prog <- Os.getProgName
                putStr $
                    "Usage: " ++ prog ++ " Command...\n"
                    ++ "\n"
                    ++ "A Command is any of these:\n"
                    ++ "\n"
                    ++ "    help        Show this.\n"
                    ++ "\n"
                    ++ "    cd DIR      Change directory to DIR.\n"
                    ++ "\n"
                    ++ "Maven commands, assuming that Maven 3 CLI is in PATH:\n"
                    ++ "\n"
                    ++ "    generate    Generate Java source code, SQL DDL file, and Dockerfile.\n"
                    ++ "    clean       Delete compilation result Java class files.\n"
                    ++ "    compile     Compile generated Java source code.\n"
                    ++ "    package     Create the JAR.\n"
                    ++ "\n"
                    ++ "Docker commands, assuming that docker CLI is in PATH:\n"
                    ++ "\n"
                    ++ "    dbuild      Build Docker image.\n"
                    ++ "    drun        Run Docker image.\n"
                    ++ "    dpush       Push Docker image to Docker registry.\n"
                    ++ "\n"
                    ++ "Kubernetes commands:\n"
                    ++ "\n"
                    ++ "    ksvc        List the app's service.\n"
                    ++ "    ksvcs       List all services.\n"
                    ++ "\n"
                    ++ "Database commands:\n"
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
            Clean -> maven ["clean"]
            Generate -> UGJ.generate_java app
            ReadPg -> SC.test
            Compile -> maven_recompile
            Package -> maven_package
            Dbuild -> docker_build
            Drun -> docker_run
            Dpush -> docker_push
            Ksvc -> kube_service
            Ksvcs -> kube_services

        in_app_dir :: IO a -> IO a
        in_app_dir = Os.withcwd dir
            where
                dir = UGJ.get_pom_xml_dir app

        -- String

        docker_tag = JWA.get_artifact_id app
        docker_repository = JWA.get_docker_repository app
        kube_service_name = JWA.get_kube_service_name app

        -- [Os.Arg] -> IO ()

        maven args = in_app_dir $ Os.call "mvn" args

        deploy :: Doc.Deploy
        deploy = Doc.mk_deploy_or_error docker_tag docker_repository

        get_kube_services :: IO [Service]
        get_kube_services = do
            bs <- Http.get_ByteString $ kube_api_server ++ "/api/v1/services"
            Right json <- return $ Json.decode bs
            return (
                (json `at` "items")
                |> Json.assume_array
                |> map (\ s ->
                    let
                        spec = s `at` "spec"
                        metadata = s `at` "metadata"
                    in
                        MkService {
                            _sName = Json.assume_string $ metadata `at` "name"
                            , _sClusterIp = Json.assume_string $ spec `at` "clusterIP"
                        }
                    )
                )
            where
                at = Json.at
                kube_api_server = "http://localhost:8001"

        display_service :: Service -> String
        display_service MkService{..} = P.render $ P.text _sName P.<> P.text ":" P.<+> P.text _sClusterIp

        -- IO ()

        maven_recompile = maven ["clean", "compile"]
        maven_package = maven ["-Prelease", "package"]
        docker_build = in_app_dir $ Doc.build deploy
        docker_run = in_app_dir $ Doc.run deploy
        docker_push = in_app_dir $ do
            putStr $
                "If docker push fails, try running \"aws ecr get-login --no-include-email\", and run the generated \"docker login\" command.\n"
                ++ "If you are using old versions of AWS CLI and Docker, you may need to remove the \"--no-include-email\" flag.\n"
            Doc.push deploy
        kube_service = do
            svcs <- get_kube_services
            putStr $ unlines $ map display_service $ filter (\ MkService{..} -> _sName == kube_service_name) svcs
        kube_services = do
            svcs <- get_kube_services
            putStr $ unlines $ map display_service svcs

data Service
    = MkService {
        _sName :: String
        , _sClusterIp :: String
    } deriving (Read, Show)

data Command
    = Nop
    | Seq Command Command
    | Setcwd FilePath
    | Clean
    | Generate
    | Help
    | ReadPg
    | Compile
    | Package
    | Dbuild
    | Drun
    | Dpush
    | Ksvc
    | Ksvcs
    deriving (Read, Show)
