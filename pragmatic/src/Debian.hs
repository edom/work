{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}

{- |
Mirroring a Debian repository.
-}
module Debian where

import Prelude ()
import Meta.Prelude

import qualified Control.Exception as Ex
import qualified Control.Monad as M
import qualified Data.List as L
import qualified System.IO as Si
import qualified System.IO.Error as Ie

import qualified Network.URI as Nu

import qualified Data.Map as Map

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bsl

import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import qualified Data.Text.Encoding.Error as Tee

import qualified System.Process as Proc

import qualified Codec.Compression.GZip as Gzip

import qualified Debian.Sources as Ds
import qualified Debian.Relation as Dr
import qualified Debian.Release as Drls

import qualified Debian_internal as Di

-- * Copying packages across repositories

{- |
@copy src dst pkgs@ copies the specified packages
from the @src@ repository to the @dst@ local directory.

Example: if @src@ is @http:\/\/archive.ubuntu.com\/ubuntu@
and @dst@ is @\/tmp\/mirror@, then some files whose path is
@http:\/\/archive.ubuntu.com\/ubuntu\/PATH@
will be copied to @\/tmp\/mirror\/archive.ubuntu.com\/ubuntu\/PATH@.

The source repository @src@ must be a remote repository.
The source packages will be downloaded using 'wget'.

The destination @dst@ must be a local directory.

If a destination file exists but older than its corresponding source file,
the destination file is renewed.
-}
copy :: Repository -> FilePath -> [Package] -> IO ()
copy rs workdir ps =
    wget workdir src_paths
    where
        src_paths = map (abs_path rs) ps
        abs_path repo pkg = r_base_uri repo ++ "/" ++ p_filepath pkg

-- * Loading package lists and finding packages

{- |
This only works for local file system.

Load binary-amd64 @Packages@ index files.
-}
load :: [Source] -> IO [Package]
load sources =
    fmap concat $ sequence $ map parse_Packages_gz_file pgz_paths
    where
        pgz_paths =
            map
                (\ s -> s_base s ++ "/dists/" ++ s_dist s ++ "/" ++ s_sect s ++ "/binary-amd64/Packages.gz")
                sources

        parse_Packages_gz_file :: FilePath -> IO [Package]
        parse_Packages_gz_file path =
            parse_Packages . Gzip.decompress . Bsl.fromStrict <$> Bs.readFile path

        parse_Packages :: (To_text a) => a -> [Package]
        parse_Packages input = packages
            where
                paratexts = T.splitOn (T.pack "\n\n") (to_text input)
                paraliness = map (T.splitOn (T.pack "\n")) paratexts
                keyvalss = map (map keyval_from_paraline) paraliness
                packages = [ p | Just p <- map package_from_keyvals keyvalss ]
                package_from_keyvals :: [(T.Text, T.Text)] -> Maybe Package
                package_from_keyvals m =
                    Mk_package
                        <$> required "Package"
                        <*> required "Version"
                        <*> required "Architecture"
                        <*> required "Filename"
                        <*> (required "Depends" >>= either_to_maybe . Dr.parseRelations)
                    where
                        required k = T.unpack . snd <$> L.find (\ (u, _) -> T.pack k == u) m
                either_to_maybe = either (const Nothing) Just
                keyval_from_paraline pl = (k, T.drop (T.length sep) v)
                    where
                        (k, v) = T.breakOn sep pl
                        -- XXX This is fragile.
                        sep = T.pack ": "

-- | Find packages by names.
find :: [String] -> Package_name_map -> [Package]
find names world =
    concatMap (\ name -> Map.findWithDefault [] name world) names

-- * Source

{- |
Corresponds to a set of Packages.gz files.

A line in APT sources.list file can expand to many 'Source's.
-}
data Source
    = Mk_source
    {
        s_base :: String -- ^ Example: @http://archive.ubuntu.com/ubuntu@
        , s_dist :: String -- ^ Example: @trusty@, @trusty-updates, @trusty-backports@, @trusty-security@
        , s_sect :: String -- ^ Example: @main@, @restricted@, @universe@, @multiverse@
    }
    deriving (Read, Show)

s_new :: Source
s_new = Mk_source "" "" ""

-- * Transmogrifying interface mismatch from @debian@

import_ :: Ds.DebSource -> [Source]
import_ (Ds.DebSource Ds.Deb uri (Right (release_name, sections))) =
    [
        s_new
        {
            s_base = Nu.uriToString id uri ""
            , s_dist = Drls.relName release_name
            , s_sect = sect
        }
        | Drls.Section sect <- sections
    ]
-- XXX Left?
import_ _ = []

slurp_sources_list_file :: FilePath -> IO [Source]
slurp_sources_list_file = fmap (concatMap import_ . Ds.parseSourcesList) . slurp

-- * Package

data Package
    = Mk_package
    {
        p_name :: String
        , p_version :: String
        , p_arch :: String
        , p_filepath :: String
        , p_depends :: Dr.Relations
    }
    deriving (Read, Show)

p_same_name :: Package -> Package -> Bool
p_same_name p q = p_name p == p_name q

-- * Repository

{- |
One repository corresponds to one directory in Debian repository format.

A repository has two child folders: @dists@ and @pool@.

@dists@ contains indexes (Packages.gz etc.).

@pool@ contains deb files (the packages themselves).
-}
data Repository
    = Mk_repository
    {
        r_base_uri :: String -- ^ This can also be a local directory path.
    }
    deriving (Read, Show)

r_new :: Repository
r_new = Mk_repository ""

-- * Package name map

type Package_name_map = Map.Map String [Package]

from_list :: [Package] -> Package_name_map
from_list =
    Map.fromListWith (++) . map (\ p -> (p_name p, [p]))

-- * IO

read_remaining :: Si.Handle -> IO String
read_remaining h =
    Ex.bracket
        (Si.hGetBuffering h)
        (Si.hSetBuffering h)
        (\ _ -> Si.hSetBuffering h Si.LineBuffering >> loop)
        -- LineBuffering is required for raising IOError
        -- when Ctrl-D is pressed at the beginning of line.
    where
        loop =
            Ie.tryIOError (Si.hGetChar h) >>= \case
                Left e | Ie.isEOFError e -> return ""
                Left e -> Ie.ioError e
                Right c -> (c :) <$> loop

-- * Ubuntu 14.04 (trusty)

ubuntu_repository :: Repository
ubuntu_repository =
    r_new
    {
        r_base_uri = "http://archive.ubuntu.com/ubuntu"
    }

ubuntu_sources :: [Source]
ubuntu_sources =
    [
        s_new
        {
            s_base = r_base_uri ubuntu_repository
            , s_dist = dist
            , s_sect = sect
        }
        | dist <- map ("trusty" ++) ["", "-updates", "-backports", "-security"]
        , sect <- ["main", "restricted", "universe", "multiverse"]
    ]

-- * Mirroring anything over HTTP/FTP with wget

{- |
For each URI in @src_uris@, the program @wget dst_root src_uris@
downloads the resources identified by the URI to @dst_root@
if the resource is newer than the local copy,
along with the intermediate path components.

For example, @wget \"\/tmp\/mirror\" [\"http:\/\/www.example.com\/index.html\"]@
will download @http:\/\/www.example.com\/index.html@ to
@\/tmp\/mirror\/www.example.com\/index.html@.

See also the man page for @wget@,
especially the @-x@ and the @-m@ options.

This requires the @wget@ command-line tool
to be installed in anywhere in the search PATH.
On Debian-based systems (like Ubuntu) you can install @wget@ by
executing @apt-get install wget@ in the terminal as root.
-}
wget :: FilePath -> [String] -> IO ()
wget dst_root src_uris =  do
    (_, _, _, proc) <- Proc.createProcess (Proc.proc "wget" args)
        {
            Proc.cwd = Just dst_root
            , Proc.std_in = Proc.Inherit
            , Proc.std_out = Proc.Inherit
            , Proc.std_err = Proc.Inherit
        }
    M.void $ Proc.waitForProcess proc
    where
        args = ["-x", "-m"] ++ src_uris

-- * Internals

class To_text a where to_text :: a -> T.Text

instance To_text T.Text where to_text = id
instance To_text String where to_text = T.pack
instance To_text Bs.ByteString where to_text = Te.decodeUtf8With Tee.lenientDecode
instance To_text Bsl.ByteString where to_text = to_text . Bsl.toStrict

-- * Resolving dependencies (don't do this; just let @apt@ do this)

{- |
Find all dependencies of a package recursively, including the package itself.

See also the caveat of 'direct_dependencies'.
-}
resolve :: [Package] -> Package -> [Package]
resolve world pkg =
    loop [] [pkg]
    where
        -- r is the list of packages that have already been processed.
        -- ps is the list of the packages to be processed.
        loop rs [] = rs
        loop rs ps =
            loop (unprocessed_deps ++ rs) unprocessed_deps
            where
                deps = concatMap (direct_dependencies world) ps
                unprocessed_deps = filter (\ p -> not $ any (p_same_name p) rs) deps

{- |
@direct_dependencies world pkg@ returns all packages in @world@ that @pkg@ depends on.
The result does not include the package itself.

Caveat: This underconstrains. This may return more dependencies than necessary.
-}
direct_dependencies :: [Package] -> Package -> [Package]
direct_dependencies world pkg = do
    Dr.Rel (Dr.BinPkgName dep_name) _ _ <- concat $ p_depends pkg
    filter (\ p -> p_name p == dep_name) world
