{- |
Interpret Java source code.
-}
module Java_parse
where

import Data.Monoid (Monoid, (<>))

import qualified Control.Monad as M
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Foreign as F
import qualified Language.Java.Parser as J
import qualified Language.Java.Syntax as S
import qualified System.Directory as D
import qualified System.IO.Error as E

parseFile :: FilePath -> IO S.CompilationUnit
parseFile path = do
    bytes <- B.readFile path
    text <- implicit $ TE.decodeUtf8' bytes
    let string = T.unpack text
    implicit $ J.parser J.compilationUnit string
    where
        implicit :: (Show e) => Either e a -> IO a
        implicit = either (E.ioError . E.userError . show) pure

filesIn :: FilePath -> IO [FilePath]
filesIn root =
    filter (not . special) <$> D.getDirectoryContents root
    where
        special x = x `elem` [".", ".."]

filesUnder :: FilePath -> IO [FilePath]
filesUnder root = do
    children <- filesIn root
    subdirs <- M.filterM D.doesDirectoryExist $ map (\ child -> root ++ "/" ++ child) children -- should use filepath?
    others <- concat <$> mapM filesUnder subdirs
    return $ children ++ others

javaFilesUnder :: FilePath -> IO [FilePath]
javaFilesUnder root = filter isJavaFile <$> filesUnder root
    where
        isJavaFile name = ".java" `L.isSuffixOf` name

type Name = String

data Namespace
    = Class
    | Field
    | Method
    | Local
    deriving (Show)

data Binding
    = MkBinding
    {
        namespace :: Namespace
        , name :: String
        , value :: Value
    }
    deriving (Show)

type Context = [Binding]

emptyContext :: Context
emptyContext = []

-- | Interpreter state.
data State
    = MkState
    {
        bindings :: Context
        , program :: Program
    }
    deriving (Show)

initState :: State
initState = MkState [] Null

data Type
    = TInt8
    | TInt16
    | TInt32
    | TInt64
    | TObject
    deriving (Show)

-- value-level name X is bound to #0.X
-- type-level name X is bound to #1.X

step :: State -> State
step state =
    case program state of
        Get (Object context) name ->
            modifyProgram (\ _ -> get name context) state
        Get prog name ->
            modifyProgram (\ p -> Get p name) $ step state
        Set (Object context) name prog ->
            modifyProgram (\ _ -> Object $ set name prog context) state
        Set owner name replacement ->
            modifyProgram (\ p -> Set p name replacement) . step . modifyProgram (\ _ -> owner) $ state
        Sequence x@(Error message) _ ->
            modifyProgram (\ _ -> x) state
        Sequence x y | irreducible x ->
            modifyProgram (\ _ -> y) state
        Sequence x y ->
            modifyProgram (\ p -> Sequence p y) . step . modifyProgram (\ _ -> x) $ state
        GetBinding name ->
            case filter (\ (n, _) -> n == name) (bindings state) of
                (_, x) : _ -> state { program = x }
                _ -> state { program = Error ("undefined: " ++ name) }
        SetBinding name prog | irreducible prog ->
            state { bindings = set name prog (bindings state), program = Null }
        SetBinding name prog ->
            modifyProgram (\ p -> SetBinding name p) . step . modifyProgram (\ _ -> prog) $ state
        Add (Int32 x) (Int32 y) ->
            modifyProgram (\ _ -> Int32 (x + y)) state
        Add (PString x) (PString y) ->
            modifyProgram (\ _ -> PString (x ++ y)) state
        Add x y ->
            let
                state0 = step state { program = x }
                state1 = step state0 { program = y }
            in
                state1 { program = Add (program state0) (program state1) }
        PopBinding ->
            state { bindings = tailOrId (bindings state), program = Null }
        Coroutine Return continuation ->
            state { program = continuation }
        Coroutine body continuation ->
            let
                state0 = step state { program = body }
            in
                state0 { program = Coroutine (program state0) continuation }
        other ->
            id $ state
        where
            modifyProgram f s = s { program = f (program s) }
            irreducible x = case x of
                Null -> True
                Error _ -> True
                Int32 _ -> True
                PString _ -> True
                Object _ -> True
                _ -> False

class MonadState m where
    getLocal :: Name -> m Value
    setLocal :: Name -> Value -> m ()
    enter :: m ()
    leave :: m ()

tailOrId (_:x) = x
tailOrId [] = []

class Pretty a where
    pretty :: a -> String

instance Pretty Program where
    pretty prog = case prog of
        Sequence x y -> pretty x ++ "\n" ++ pretty y
        SetBinding name value -> name ++ " := " ++ pretty value
        GetBinding name -> name
        Object bindings -> "{" ++ L.intercalate ", " (map (\ (name, value) -> name ++ ": " ++ pretty value) bindings) ++ "}"
        Set owner name val -> "(" ++ pretty owner ++ "." ++ name ++ " := " ++ pretty val ++ ")"
        other -> show other

{- |
All references are canonical.
-}
data Program
    = Constant Value
    | PopBinding
    | Coroutine Program Program -- ^ program, continuation
    | Return -- ^ jump to continuation
    | Sequence Program Program
    | Add Program Program -- ^ integer addition or string concatenation
    | Get Program Name -- ^ get field from object
    | Set Program Name Program -- ^ set object field
    | GetBinding Name  -- ^ get from binding
    | SetBinding Name Program -- ^ replace a binding
    deriving (Show)

{- |
Irreducible.
-}
data Value
    = Null
    | Error String
    | Int32 I.Int32
    | PString String
    | Object [Binding] -- ^ a class, an object, the variables captured by a closure, are contexts (set of bindings)
    | Stored Program
    deriving (Show)

instance Monoid Program where
    mempty = Null
    mappend = Sequence

set :: Name -> Program -> Context -> Context
set name expr [] = [(name, expr)]
set name expr ((n, x) : y) | n == name = (n, expr) : y
set name expr (x : y) = x : set name expr y

get :: Name -> Context -> Program
get name ((n, x) : _) | n == name = x
get name (_ : y) = get name y
get name [] = Null

main = do
    rep 20 initState { program =
            SetBinding "Foo" (Object [("x", Int32 0)])
            <> SetBinding "Foo" (Set (GetBinding "Foo") "y" (Int32 1))
            -- <> SetBinding "Foo" (Set (GetBinding "Foo") "greet" (Lambda "name" (Add (GetBinding "name") (PString ", hi!"))))
            <> SetBinding "name" (PString "hi")
            <> Coroutine
                (Sequence
                    (Add (GetBinding "name") (PString ", hi!"))
                    Return
                )
                PopBinding
        }
    where
        rep n s | n >= (0 :: Int) = do
            -- putStrLn $ "#" ++ show n ++ "\n" ++ pretty (program s) ++ "\n"
            putStrLn $ "#" ++ show n ++ "\n" ++ show (program s) ++ "\n" ++ show (bindings s) ++ "\n"
            let s0 = step s
            rep (n - 1) s0
        rep _ s = return s

{-
main = do
    files <- javaFilesUnder "."
    units <- mapM parseFile files
    mapM_ print units
-}

-- main = J.parser J.compilationUnit "import java.util.List; class Foo {}"
