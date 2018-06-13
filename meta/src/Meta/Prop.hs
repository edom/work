module Meta.Prop where

import qualified System.IO.Error as E

-- * Application

{- |
Flipped '$'.

https://github.com/izdi/elm-cheat-sheet
-}
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

-- * Error handling

{- |
The only relevant instance of this class is 'Err'.

With applicatives, we go as far as possible while encountering errors.

With monads, we go as short as possible while encountering errors.
-}
class (Applicative f) => AppliError f where
    raise :: [String] -> f a

-- | Go as far as we can instead of stopping at the first error.
tryHard :: [Either [e] a] -> Either [e] [a]
tryHard comps = case comps of
    [] -> Right []
    Left h : t -> Left $ h ++ either id (const []) (tryHard t)
    Right h : t -> case tryHard t of
        Left hh -> Left hh
        Right tt -> Right $ h : tt

{- |
Use @ApplicativeDo@ language extension.
-}
data Err a
    = ELeft [String]
    | ERight a
    deriving (Read, Show)

instance Functor Err where
    fmap _ (ELeft x) = ELeft x
    fmap f (ERight x) = ERight (f x)

instance Applicative Err where
    pure = ERight
    (<*>) ff fx = case ff of
        ELeft ef -> case fx of
            ELeft ex -> ELeft (ef ++ ex)
            ERight _ -> ELeft ef
        ERight f -> case fx of
            ELeft ex -> ELeft ex
            ERight x -> ERight (f x)

instance AppliError Err where
    raise = ELeft

instance Monad Err where
    return = pure
    (>>) ma mb = ma *> mb
    (>>=) mx k = case mx of
        ELeft e -> ELeft e
        ERight x -> k x
    fail x = raise [x]

getErrors :: Err a -> [String]
getErrors (ELeft x) = x
getErrors (ERight _) = []

eitherFromErr :: Err a -> Either [String] a
eitherFromErr m = case m of
    ELeft x -> Left x
    ERight x -> Right x

errFromEither :: Either [String] a -> Err a
errFromEither m = case m of
    Left x -> ELeft x
    Right x -> ERight x

ioFromErr :: Err a -> IO a
ioFromErr = ioFromEither . eitherFromErr

ioFromEither :: Either [String] a -> IO a
ioFromEither e = case e of
    Left errs -> do
        putStr $ unlines errs
        E.ioError $ E.userError "There are compilation errors."
    Right x -> pure x

-- * Properties

-- ** Name

class GetName a b | a -> b where getName :: a -> b
class SetName a b | a -> b where setName :: b -> a -> a

-- ** Type

class GetType a b | a -> b where getType :: a -> b
class SetType a b | a -> b where setType :: b -> a -> a
