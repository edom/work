{-# LANGUAGE LambdaCase #-}

-- | Dependency checking.

module Dependency where

import qualified Control.Applicative as A
import qualified Control.Exception as Ex
import qualified Control.Monad as M
import qualified Data.List as L
import qualified System.IO.Error as Ie

import qualified Data.Time as Ti

import qualified File as F

-- * Making Items

load :: FilePath -> IO Item
load path = do
    Ex.try (Mk_item path True A.<$> F.getModificationTime path) >>= \case
        Left e | Ie.isDoesNotExistError e ->
            Mk_item path False <$> Ti.getCurrentTime
        Left e ->
            Ex.throwIO e
        Right i ->
            return i

type World = [Item]

load_under :: FilePath -> IO World
load_under dir =
    F.files_under dir
    >>= M.mapM load . filter (`F.extensionIs` ".md")

-- * Declaring dependency

{- |
@depends_on a b@ (or its infix form @a \`depends_on\` b@)
states that @a@ depends on @b@; typically @a@ is the output
and @b@ is an input for creating @a@.
-}
depends_on :: FilePath -> [FilePath] -> Dependency
depends_on output inputs = Mk_dependency output inputs update
    where
        update = return ()

-- * Computing dependency

compute_need_update :: [Dependency] -> IO [Dependency]
compute_need_update deps = do
    world <- L.nubBy (\ x y -> i_path x == i_path y)
        <$> mapM load (concatMap dep_paths deps)
    return $ map fst $ filter (is_update . snd) $ compute_0 world deps
    where
        dep_paths :: Dependency -> [FilePath]
        dep_paths d = d_output d : d_inputs d
        is_update (Update _) = True
        is_update _ = False

compute_0 :: World -> [Dependency] -> [(Dependency, Result)]
compute_0 world deps = do
    dep <- deps
    let
        output_path = d_output dep
        input_paths = d_inputs dep
    output <- take 1 $ filter (\ i -> i_path i == output_path) world
    -- TODO if output Item is not in World, report error in Result
    let
        inputs = filter (\ i -> i_path i `elem` input_paths) world
        changers = filter (`newer_than` output) inputs
    return $ case () of
        _ | not (i_exists output) -> (dep, Update OutputNotExist)
          | not (null changers) -> (dep, Update $ InputChanged $ map i_path changers)
        _ -> (dep, Keep)

newer_than :: Item -> Item -> Bool
newer_than x y = i_mtime x > i_mtime y

-- * Result

-- | Result of dependency checking.
data Result
    = Keep -- ^ The item does not need to be recomputed.
    | Update Reason -- ^ The item needs to be recomputed.
    deriving (Show, Read)

data Reason
    = OutputNotExist
    | InputChanged [FilePath]
    deriving (Show, Read)

-- * Internals

data Dependency
    = Mk_dependency
    {
        d_output :: FilePath
        , d_inputs :: [FilePath]
        , d_update :: IO ()
    }

data Item
    = Mk_item
    {
        i_path :: FilePath
        , i_exists :: Bool
        , i_mtime :: Ti.UTCTime
    }
    deriving (Show, Read)

-- * Convenience functions

{- |
Check if the output needs to be recomputed;
find out which inputs are newer than the output.

If any of the inputs change, the output changes.
-}
dependsOn
    :: FilePath -- ^ output
    -> [FilePath] -- ^ list of inputs; should not be empty
    -> IO Result

dependsOn output inputs = do
    outputExists <- F.doesFileExist output
    if outputExists
        then depOn A.<$> load output A.<*> M.mapM load inputs
        else return $ Update OutputNotExist

-- * Logic

-- | TODO name this function
depOn :: Item -> [Item] -> Result
depOn output inputs =
    case changedInputs of
        [] -> Keep
        x -> Update $ InputChanged $ map i_path x
    where
        changedInputs = filter (`newer_than` output) inputs
