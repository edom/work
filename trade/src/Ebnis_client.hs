{-# LANGUAGE RecordWildCards #-}

module Ebnis_client (
    Config(..)
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

main :: IO ()
main = P.withSocketsDo $ do
    -- This doesn't work, but this 'openssl -inform der -in trade/what.der' works.
    let str = "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDVd/gb2ORdLI7nTRHJR8C5EHs4RkRBcQuQdHkZ6eq0xnV2f0hkWC8h0mYH/bmelb5ribwulMwzFkuktXoufqzoft6Q6jLQRnkNJGRP6yA4bXqXfKYj1yeMusIPyIb3CTJT/gfZ40oli6szwu4DoFs66IZpJLv4qxU9hqu6NtJ+8QIDAQAB"
    let bas = fromString str
    der <- P.base64_decode bas
    B.writeFile "what.der" der
    rpk <- P.rsa_read_public_key_from_der_string der
    putStr $ show rpk

    cnf_path <- maybe def_cnf_path id <$> Os.lookupEnv "CNF_FILE"
    P.log $ "Reading configuration from file: " ++ cnf_path
    MkConfig{..} <- read_config_from_file cnf_path
    user <- putStr "User: " >> getLine
    pass <- putStr "Password: " >> get_password
    lbs_uuid <- B.fromStrict <$> P.getRandomBytes 16
    uuid <- maybe (fail "Should not happen: Could not generate UUID.") return $ U.fromByteString lbs_uuid
    let session_key = U.toASCIIBytes uuid
        con = C.def_connection {
                C._login = user
                , C._passcode = pass
                , C._session_key = session_key
            }
    bs_con <- P.encode_connect _server_public_key con
    P.with_connect _server_address "62229" $ \ sock _ -> do
        P.send sock bs_con
        putStrLn "Not implemented."
    where
        def_cnf_path = "cnf/ebnis.yaml"
