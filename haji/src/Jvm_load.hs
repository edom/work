{- |
This module deals with loading classes without initializing them.
-}
module Jvm_load
where

import qualified Control.Applicative as Ap
import qualified Control.Monad as Mo
import qualified System.IO.Error as Ie

import qualified Control.Monad.IO.Class as Ic

import qualified Data.ByteString.UTF8 as Bu

import qualified Jvm_arch as A
import qualified Jvm_io as Z
import qualified Jvm_prepare as P

-- * Loading classes

class Load m where

    {- |
This does not run the class initializer.

This does not add the class to the list of loaded classes.
    -}
    load_class :: A.Class_name -> m A.Class

instance Load A.S where

    load_class = A.get_loaded_class

instance Load A.J where

    load_class name = do
        classpath <- A.gets A.s_classpath
        m <- A.find_loaded_class name
        case m of
            Just x -> return x
            _ -> load_class_in name classpath

-- * Loading classes in classpath

load_class_in :: (A.Stateful m, Ic.MonadIO m) => A.Class_name -> [FilePath] -> m A.Class
load_class_in name [] = A.stop (A.Class_not_found name)
load_class_in name (dir : rest) = do
    -- FIXME SECURITY path traversal
    -- TODO use filepath package
    let path = dir ++ "/" ++ Bu.toString name ++ ".class"
    debug_load path
    ec <- Ic.liftIO $ Ie.tryIOError $ load_class_file path
    case ec of
        Left e | Ie.isDoesNotExistError e -> load_class_in name rest
        Left e -> A.stop (A.Failed_loading_class name (show e))
        Right (Left e) -> A.stop (A.Failed_loading_class name e)
        Right (Right c) -> return c
    where
        debug_load path = do
            debug <- A.gets A.s_debug_load
            Mo.when debug $ Ic.liftIO $ putStrLn $ "[debug] [Jvm_load] [load_class] Trying " ++ path

-- * Internals

{- |
This returns a 'A.Class' from "Jvm_arch", not a 'Z.Class' from "Jvm_io".

You are not supposed to use this function directly;
let the virtual machine load the classes on its own.
-}
load_class_file :: FilePath -> IO (Either String A.Class)
load_class_file = fmap (>>= P.resolve_class) . parse_class_file

{- |
This returns a 'Z.Class' from "Jvm_io", not a 'A.Class' from "Jvm_arch".

Deserialize the binary representation from disk.
-}
parse_class_file :: FilePath -> IO (Either String Z.Class)
parse_class_file path = Z.parse_class path <$> Z.slurp path
