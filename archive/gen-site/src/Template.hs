module Template
where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Monoid as Mo
import qualified Data.Typeable as T

import qualified System.FilePath as Fp

import qualified Dictionary as D
import qualified Metadata as Md
import qualified Hakyll.Web.Template.Internal as I

-- * Rendering

readTemplateFile :: FilePath -> IO I.Template
readTemplateFile path = I.readTemplate <$> readFile path

applyFile :: TemplateFilePath -> Dic -> IO String
applyFile path dic = readTemplateFile path >>= apply path dic

{- |
Substitute the keys in the template
with the matching values in the dictionary.

The file path should be an absolute path.

Backward incompatibility:

New behavior: \$partial\$ is relative to the current file.

Old behavior: \$partial\$ is relative to the input root directory.
-}
apply :: TemplateFilePath -> Dic -> I.Template -> IO String
apply templatePath dic tem =
    resolve templatePath dic tem >>= applyResolved dic

-- * Dictionary constructors

{- $
Each constructor creates a one-entry dictionary.

To make a dictionary that has many entries, use 'mappend'.

In the expression @'mappend' left right@,
the entries in @left@ override those in @right@.
-}

literal :: (Eq k, Show k) => k -> v -> D.Dictionary k (Field v)
literal key val = D.MkDictionary $ \ k ->
    if k == key
        then return $ Literal val
        else fail $ "Template: no such key: " ++ show k

function :: (Eq k, Show k) => k -> ([String] -> Either String v) -> D.Dictionary k (Field v)
function key fun = D.MkDictionary $ \ k ->
    if k == key
        then return $ Function fun
        else fail $ "Template: no such key: " ++ show k

list :: (Eq k, Show k) => k -> [D.Dictionary String (Field v)] -> D.Dictionary k (Field v)
list key dics = D.MkDictionary $ \ k ->
    if k == key
        then return $ List dics
        else fail $ "Template: no such key: " ++ show k

{- |
The same key maps to the stringified value.
-}
metadata :: Md.Metadata -> Dic
metadata m = D.MkDictionary $ \ k -> fmap stringify $ D.lookup k m
    where
        stringify x = case x of
            Md.String y -> Literal y
            Md.Bool y -> Literal $ show y
            Md.List y -> Literal $ show y

type Dic = D.Dictionary String (Field String)

-- * Internals

data Field v
    = Literal v
    | List [D.Dictionary String (Field v)]
    | Function ([String] -> Either String v)

{- |
This deals with string substitution but without 'IO'.

This is 'I.Template' without 'I.Partial'.
-}
type ResolvedTemplate = [ResolvedTemplateElement]
data ResolvedTemplateElement
    = Chunk String
    | Expr I.TemplateExpr
    | Escaped
    | If I.TemplateExpr ResolvedTemplate (Maybe ResolvedTemplate)   -- expr, then, else
    | For I.TemplateExpr ResolvedTemplate (Maybe ResolvedTemplate)  -- expr, body, separator
    deriving (Show, Eq, T.Typeable)

-- | Each inhabitant of this type should be an absolute path.
type TemplateFilePath = FilePath

resolve
    :: TemplateFilePath -- ^ must be absolute
    -> Dic
    -> I.Template
    -> IO ResolvedTemplate
resolve templatePath dic template = go 0 (I.unTemplate template)
    where
        go :: Int -> [I.TemplateElement] -> IO ResolvedTemplate
        go depth _ | depth >= (8 :: Int) =
            fail $ "Template: " ++ templatePath ++ ": a $partial$ is nesting too deeply; maybe there is a cycle"
        go _ [] = return []
        go depth (u : y) = do
            x <- case u of
                I.Chunk a -> return [Chunk a]
                I.Expr e -> return [Expr e]
                I.Escaped -> return [Escaped]
                I.If e t mf -> do
                    t' <- go depth t
                    mf' <- maybe (return Nothing) (fmap Just . go depth) mf
                    return [If e t' mf']
                I.For e b ms -> do
                    b' <- go depth b
                    ms' <- maybe (return Nothing) (fmap Just . go depth) ms
                    return [For e b' ms']
                I.Partial pathexpr -> do
                    path <- applyExpr dic pathexpr >>= wantLiteral pathexpr
                    let resolvedPath = Fp.takeDirectory templatePath Fp.</> path
                    tem <- I.readTemplate <$> readFile resolvedPath
                    go (depth + 1) $ I.unTemplate tem
            z <- go depth y
            return $ x ++ z

applyResolved :: (A.Alternative m, Monad m) => Dic -> ResolvedTemplate -> m String
applyResolved dic =
    M.foldM (\ a elm -> (a ++) A.<$> applyElem dic elm) ""

applyElem :: (A.Alternative m, Monad m) => Dic -> ResolvedTemplateElement -> m String
applyElem dic tel = case tel of
    Chunk s -> return s
    Expr expr -> applyExpr dic expr >>= wantLiteral expr
    Escaped -> return "$"
    If e t mf -> do
        cond <- (applyExpr dic e >> return True) A.<|> return False
        if cond
            then applyResolved dic t
            else maybe (return "") (applyResolved dic) mf
    For e b s -> applyExpr dic e >>= \ cf -> case cf of
        List dics -> do
            sep <- maybe (return "") (applyResolved dic) s
            bs  <- M.mapM (\ d -> applyResolved (d Mo.<> dic) b) dics
            return $ L.intercalate sep bs
        _  -> fail $
            "Template: expecting List but got " ++ show e

applyExpr :: (A.Alternative m, Monad m) => Dic -> I.TemplateExpr -> m (Field String)
applyExpr dic expr = case expr of
    I.Ident (I.TemplateKey k) ->
        maybe (keyNotFound k) return $ D._unDictionary dic k

    I.Call (I.TemplateKey k) args -> do
        strargs <- M.mapM (\ e -> applyExpr dic e >>= wantLiteral e) args
        fun <- maybe (keyNotFound k) (wantFunction expr) $ D._unDictionary dic k
        E.either fail (return . Literal) $ fun strargs

    I.StringLiteral s -> return $ Literal s
    where
        keyNotFound k = fail $ "Template: key not found: " ++ show k
        wantFunction _ (Function x) = return x
        wantFunction e _ = fail $ "Template: expecting Function; got " ++ show e

wantLiteral :: (Monad m) => I.TemplateExpr -> Field v -> m v
wantLiteral _ (Literal x) = return x
wantLiteral e _ = fail $ "Template: expecting Literal; got " ++ show e
