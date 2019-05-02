{-# LANGUAGE LambdaCase #-}

module Main where

import qualified System.Environment as Env
import qualified System.IO as Si

import qualified Debian as D

main :: IO ()
main = return ()

find_and_copy :: [FilePath] -> IO ()
find_and_copy wanted_pkgnames = do
    all_pkgs <- D.load local_sources
    let wanted_pkgs = D.find wanted_pkgnames (D.from_list all_pkgs)
    D.copy D.ubuntu_repository "../apt-manual-mirror/content" wanted_pkgs

data Cnf
    = Mk_cnf
    {
        c_help :: Bool
        , c_sources_list :: FilePath
        , c_mirror_base :: FilePath
    }
    deriving (Read, Show)

cnf_new :: Cnf
cnf_new = Mk_cnf False "" ""

parse_args :: [String] -> Cnf
parse_args = loop cnf_new
    where
        loop cnf = \case
            [] -> cnf
            "-h" : r -> loop cnf { c_help = True } r
            "-s" : path : r -> loop cnf { c_sources_list = path } r
            "-b" : path : r -> loop cnf { c_mirror_base = path } r
            _ : r -> loop cnf r

-- * Printing URIs for downloading, given list of packages

-- | FIXME Debian.Sources.parseSourcesList uses error
print_uris :: IO ()
print_uris = do
    cnf_ <- parse_args <$> Env.getArgs
    let cnf = cnf_ { c_sources_list = "/etc/apt/sources.list" }
    case c_help cnf of
        True ->
            putStrLn help
        _ -> do
            world <- D.from_list <$> D.load local_sources
            pkglist <- words <$> D.read_remaining Si.stdin
            -- let pkglist = ["build-essential", "zzuf"]
            let pkgs = D.find pkglist world
                uris = map (\ p -> repository_base_uri ++ "/" ++ D.p_filepath p) pkgs
            print uris
            -- mapM_ print $ concatMap (resolve world) (find "gnome-terminal" world)
    where
        local_mirror = D.r_new
            {
                D.r_base_uri = "../apt-manual-mirror/content/archive.ubuntu.com"
            }
        local_root = D.r_base_uri local_mirror
        repository_base_uri = "http://archive.ubuntu.com/ubuntu"
        help = "help"

local_repository :: D.Repository
local_repository =
    D.r_new
    {
        D.r_base_uri = "../apt-manual-mirror/content/archive.ubuntu.com/ubuntu"
    }

local_sources :: [D.Source]
local_sources =
    map (\ s -> s { D.s_base = D.r_base_uri local_repository }) D.ubuntu_sources

-- | TESTING
slist = do
    D.slurp_sources_list_file "/etc/apt/sources.list" >>= mapM_ print
    return ()
