{-# LANGUAGE NoImplicitPrelude #-}

module Sound.Class
(
    -- * Pair
    Pair(..)
    , curry
    , fst
    , snd
    , mapFst
    , mapSnd
    -- * Semicategory
    , Semicategory(..)
    -- * Point
    , Point(..)
    -- * Stream
    -- ** Head
    , Head(..)
    -- ** Tail
    , Tail(..)
    -- ** Cons
    , Cons(..)
    -- ** Decons
    , Decons(..)
    -- ** DeconsM
    , DeconsM(..)
    -- ** Consume
    , Consume(..)
    , Consumer
    , Index
    , Count
    -- ** Fill
    , Fill(..)
    -- ** Scan
    , Scan(..)
    -- ** Zip2
    , Zip2(..)
    -- * HardSync
    , HardSync(..)
    , Master
    , Slave
    -- * Aprep
    , Aprep(..)
    -- * Trans
    , Trans(..)
    -- * Reexports
    , module Prelude
    , module Control.Applicative
    , module Control.Category
)
where

import Control.Applicative
import Control.Category hiding ((.))
import Foreign
import Prelude hiding (curry, fst, head, id, snd, scanl, tail, uncurry, (.))
import qualified Prelude as P

import Sound.Buffer

{- |
If f is an instance of 'Pair', then it is an instance of 'Semicategory':

@
x '.' y = 'mkPair' ('fst' y) ('snd' x)
@

Laws:

@
'mkPair' = 'curry' 'id'
@

@
'uncurry' f x = f ('fst' x) ('snd' x)
@

@
'fst' = 'uncurry' 'const'
@

@
'snd' = 'uncurry' ('flip' 'const')
@
-}
class Pair f where

    mkPair :: a -> b -> f a b

    uncurry :: (a -> b -> c) -> f a b -> c

curry :: (Pair f) => (f a b -> c) -> a -> b -> c
curry f x y = f (mkPair x y)

fst :: (Pair f) => f a b -> a
fst = uncurry const

snd :: (Pair f) => f a b -> b
snd = uncurry (flip const)

mapFst :: (Pair f) => (a -> c) -> f a b -> f c b
mapFst f = uncurry (\ x y -> mkPair (f x) y)

mapSnd :: (Pair f) => (b -> c) -> f a b -> f a c
mapSnd f = uncurry (\ x y -> mkPair x (f y))

class Semicategory f where
    (.) :: f b c -> f a b -> f a c

instance Semicategory (->) where
    (.) = (P..)

instance Pair (,) where
    mkPair = (,)
    uncurry f (x,y) = f x y
    {-# INLINE uncurry #-}

instance Semicategory (,) where
    x . y = mkPair (fst y) (snd x)
    {-# INLINE (.) #-}

class Head f where
    {- |
This generalizes the 'P.head' in "Prelude".
    -}
    head :: f a -> a

class Tail f where
    {- |
This generalizes the 'P.tail' in "Prelude".
    -}
    tail :: f a -> f a

class Cons f where cons :: a -> f a -> f a

instance Cons [] where cons = (:)

class (Head f, Tail f) => Decons f where
    {- |
@
'decons' x f = f ('head' x) ('tail' x)
@
    -}
    decons :: f a -> (a -> f a -> b) -> b
    decons x f = f (head x) (tail x)
    {-# INLINE decons #-}

class DeconsM m f where
    deconsM :: f a -> (a -> f a -> m b) -> m b

class (DeconsM IO f) => Consume f where
    consume :: Consumer a -> Count Int -> f a -> IO (f a)
    consume eat n =
        loop 0
        where
            loop i s
                | i >= n = return s
                | otherwise =
                    deconsM s $ \ h t -> do
                        eat i h
                        loop (i + 1) t
    {-# INLINE consume #-}

class (Consume f) => Fill f where
    fill :: (Storable a) => Buffer Ptr a -> f a -> IO (f a)
    fill buf = consume (pokeElemOff ptr) count
        where
            ptr = bufPtr buf
            count = bufCap buf
    {-# INLINE fill #-}
    fill_ :: (Storable a) => Buffer Ptr a -> f a -> IO ()
    fill_ b x = fill b x >> return ()

type Consumer a = Index Int -> a -> IO ()
type Index a = a
type Count a = a

class Scan f where
    {- |
This generalizes the 'P.scanl' in "Prelude".
    -}
    scanl :: (b -> a -> b) -> b -> f a -> f b

instance Scan [] where
    scanl = P.scanl

{- |
If f is an instance of 'Point' and 'Applicative',
then f must satisfy this:

@
'pure' = 'point'
@
-}
class Point f where
    point :: a -> f a

instance Point Maybe where point = Just
instance Point (Either e) where point = Right
instance Point IO where point = pure

{- |
This plus 'Point' is equivalent to 'Applicative':

* zip1 would be 'fmap',

* zip2 is 'liftA2',

* zip3 would be 'liftA3',

* and so on.

If f is an instance of 'Zip2' and 'Applicative',
then f must satisfy these:

@
('<*>') = 'zip2' ('$')
@

@
'zip2' = 'liftA2'
@

If you have Point and Zip2, you get Applicative for free.

If you have Applicative, you get Point and Zip2 for free.

You can copy this boilerplate:

@
instance Applicative T where
    pure = point
    (\<*\>) = zip2 ($)
@
-}
class Zip2 f where
    zip2 :: (a -> b -> c) -> f a -> f b -> f c

{- |
Arbitrary prepending.

This is like list but this allows the implementation to be arbitrary.
-}
data Aprep f a
    = Prep a (Aprep f a)
    | Rest (f a)
    deriving (Read, Show)

instance (Head f) => Head (Aprep f) where
    head (Prep x _) = x
    head (Rest x) = head x

instance (Tail f) => Tail (Aprep f) where
    tail (Prep _ x) = tail x
    tail (Rest x) = Rest (tail x)

instance Cons (Aprep f) where cons = Prep
instance (Decons f) => Decons (Aprep f)
instance (Decons f) => HardSync (Aprep f)

{- |
Natural transformation.

Functor homomorphism.

Laws:

@
'trans' '.' 'point' = 'point'
@

@
'fmap' f '.' 'trans' = 'fmap' f
@
-}
class Trans f g where
    trans :: f a -> g a

instance Trans f (Aprep f) where trans = Rest
-- instance Trans Stream L where trans = flip decons cons
-- instance Trans L Stream where trans = flip decons cons

-- mapM :: (a -> m b) -> Aprep f a -> Aprep f (m b)
-- foldr :: (a -> b -> b) -> b -> Aprep f a -> Aprep f b

-- stream prefix
data Fin a
    = MkFin
    {
        _fcount :: Count Int
        , _finner :: a
    }
    deriving (Read, Show)

class (Cons f, Decons f) => HardSync f where
    {- |
Reset slave when the master is true.

This may cause aliasing.

Examples:

@
master  F F F F F F F F
slave   0 1 2 3 4 5 6 7
result  0 1 2 3 4 5 6 7

master  T F F F T F F F
slave   0 1 2 3 4 5 6 7
result  0 1 2 3 0 1 2 3

master  T F T F T F T F
slave   0 1 2 3 4 5 6 7
result  0 1 0 1 0 1 0 1
@
    -}
    hardSync :: Master (f Bool) -> Slave (f a) -> f a
    hardSync master_ origSlave =
        decons origSlave $ \ oh ot ->
        let
            loop master slave =
                decons master $ \ mh mt ->
                decons slave $ \ sh st ->
                if mh
                    then cons oh (loop mt ot)
                    else cons sh (loop mt st)
        in
            loop master_ origSlave
    {-# INLINE hardSync #-}
    -- hardSyncIntBool :: Master (f IntBool) -> Slave (f a) -> f a

-- | Something controlling a 'Slave'.
type Master a = a

-- | Something being controlled by a 'Master'.
type Slave a = a
