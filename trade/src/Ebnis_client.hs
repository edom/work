module Ebnis_client (
    Config(..)
    , read_config_from_file
    , Monad_client(..)
    , main
) where

import Prelude ()
import Meta.Prelude

import qualified Meta.Text as T
import qualified Data.YAML as Y

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
            ebnis <- m Y..: fromString "ebnis"
            server <- ebnis Y..: fromString "server"
            address <- T.unpack <$> (server Y..: fromString "address")
            -- base64 of DER-form of server RSA public key
            txt_public_key <- server Y..: fromString "public_key"
            let str_public_key = T.unpack txt_public_key
                bas_public_key = fromString str_public_key
            der_public_key <- P.base64_decode bas_public_key
            public_key <- P.rsa_read_public_key_from_der_string der_public_key
            return $ MkConfig address public_key

class Monad_client m where
    get_rsa_public_key :: m P.RSA_public_key

main :: IO ()
main = putStrLn "Not implemented."
