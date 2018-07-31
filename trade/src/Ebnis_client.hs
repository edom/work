module Ebnis_client (
    Monad_client(..)
    , main
) where

import Prelude ()
import Meta.Prelude

import Meta.Crypto as Crypto

class Monad_client m where
    get_rsa_public_key :: m Crypto.RSA_public_key

main :: IO ()
main = putStrLn "Not implemented."
