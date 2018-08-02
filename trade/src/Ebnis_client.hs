{-# LANGUAGE RecordWildCards #-}

module Ebnis_client (
    Config(..)
    , read_config
    , read_config_from_file
    , Monad_client(..)
    , main
) where

import Prelude ()
import Meta.Prelude

import qualified Data.UUID as U
import qualified Data.YAML as Y
import qualified System.IO as I

import qualified Meta.ByteString as B
import qualified Meta.Os as Os
import qualified Meta.Text as T
import Ebnis_connect as C
import Ebnis_proto as P

data Config = MkConfig {
        _server_address :: String
        , _server_public_key :: P.RSA_public_key
    } deriving (Read, Show)

read_config_from_file :: FilePath -> IO Config
read_config_from_file path = do
    str <- slurp path
    cnfs <- either fail return $ Y.decode str
    case cnfs of
        [] -> fail $ "Could not read config from file: " ++ path
        cnf : _ -> return cnf

instance Y.FromYAML Config where
    parseYAML =
        Y.withMap "Config" $ \ m -> do
            config <- m Y..: fromString "config"
            ebnis <- config Y..: fromString "ebnis"
            server <- ebnis Y..: fromString "server"
            address <- T.unpack <$> (server Y..: fromString "address")
            txt_public_key <- server Y..: fromString "public_key"
            public_key <- parse_rsa_public_key $ T.unpack txt_public_key
            return $ MkConfig address public_key

-- | The input is base64 of DER-form of RSA public key.
parse_rsa_public_key :: (Monad m) => String -> m P.RSA_public_key
parse_rsa_public_key str = do
    let bas = fromString str
    der <- P.base64_decode bas
    P.rsa_read_public_key_from_der_string der

class Monad_client m where
    get_rsa_public_key :: m P.RSA_public_key

-- https://stackoverflow.com/questions/4064378/prompting-for-a-password-in-haskell-command-line-application

with_echo :: Bool -> IO a -> IO a
with_echo echo action = do
    old <- I.hGetEcho I.stdin
    bracket_ (I.hSetEcho I.stdin echo) (I.hSetEcho I.stdin old) action

get_password :: IO String
get_password = with_echo False getLine

prompt :: (MonadIO m) => String -> m ()
prompt str = do
    putStr str
    liftIO $ I.hFlush I.stdout

read_config :: IO Config
read_config = do
    cnf_path <- maybe def_cnf_path id <$> Os.lookupEnv "CNF_FILE"
    P.log $ "Reading configuration from file: " ++ cnf_path
    read_config_from_file cnf_path
    where
        def_cnf_path = "cnf/ebnis.yaml"

main :: IO ()
main = P.withSocketsDo $ do
    MkConfig{..} <- read_config
    user <- prompt "User: " >> getLine
    pass <- prompt "Password: " >> get_password
    putStr "\n"
    lbs_uuid <- B.fromStrict <$> P.getRandomBytes 16
    uuid <- maybe (fail "Should not happen: Could not generate UUID.") return $ U.fromByteString lbs_uuid
    let session_key = U.toASCIIBytes uuid
        con = (C.def_connection C.Client) {
                C._login = user
                , C._passcode = pass
                , C._session_key = session_key
            }
    bs_con <- P.encode_connect _server_public_key con
    let port = 62229 :: Word16
    P.log $ "Connecting to server: " ++ _server_address ++ ":" ++ show port
    P.with_connect _server_address (show port) $ \ sock _ -> do
        P.log $ "Sending CONNECT message."
        P.write_connect sock _server_public_key con
        let pre_session = P.mk_session sock con (fromString "0")
        session_id <- flip P.runReaderT pre_session $ do
            P.log $ "Waiting for CONNECTED message."
            frame <- P.read_frame
            message <- P.decode_message frame
            case message of
                Connected session_id -> return session_id
                _ -> fail $ show message
        let session = P.set_session_id session_id pre_session
        putStr $ "Session ID: " ++ show session_id
        putStr "\n"
