module Meta.CalIo where

import Prelude hiding (exp, getLine, putStrLn)

import qualified Prelude as P

import qualified Meta.Cal as C
import qualified Meta.CalExp as E
import qualified Meta.CalVal as V

data Io a
    = GetLine
    | PutStrLn a
    deriving (Read, Show)

instance Functor Io where
    fmap _ GetLine = GetLine
    fmap f (PutStrLn a) = PutStrLn (f a)

-- This seems to be Traversable.sequenceA.
flipA :: (Applicative f) => Io (f a) -> f (Io a)
flipA GetLine = pure GetLine
flipA (PutStrLn a) = PutStrLn <$> a

data Lang
    = LVal V.Val
    | LExp (E.Exp Lang)
    | LIo (Io Lang)
    deriving (Read, Show)

type Exp a = E.Exp a

type Val = V.Val

getLine :: Lang
getLine = LIo GetLine

putStrLn :: Lang -> Lang
putStrLn = LIo . PutStrLn

plus :: Lang -> Lang -> Lang
plus a b = LExp $ E.Plus a b

string :: String -> Lang
string = LVal . V.String

eval :: Lang -> IO Val
eval (LVal v) = pure v
eval (LExp e) =
    let
        -- e :: Exp Lang
        a :: Exp (IO Val)
        a = eval <$> e
        b :: IO (Exp Val)
        b = E.flipA a
    in
        E.eval <$> b
eval (LIo i) =
    let
        -- i : Io Lang
        a :: Io (IO Val)
        a = eval <$> i
        b :: IO (Io Val)
        b = flipA a
    in
        eval_io =<< b

eval_io :: Io Val -> IO Val
eval_io exp = case exp of
    GetLine -> V.String <$> P.getLine
    PutStrLn a -> case a of
        V.String s -> P.putStrLn s *> pure V.Unit
        _ -> pure $ V.Error ["Meta.CalIo.eval: (" ++ show exp ++ ")"]

example1 = eval $ putStrLn (plus getLine getLine)
