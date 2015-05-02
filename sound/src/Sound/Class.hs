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
    -- ** Unfold
    , Unfold(..)
    -- ** FromList
    , FromList(..)
    , fromList_cons
    , fromList_unfold
    , headWithDef
    , tailOrEmpty
    -- ** Take
    , Take(..)
    , TakeF(..)
    -- ** Cons, Decons, DeconsM
    , Cons(..)
    , Decons(..)
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
    , scanl_cons
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
    -- * Convert
    , Convert(..)
    -- * Reexports
    , module Prelude
    , module Control.Applicative
    , module Control.Category
)
where

import Control.Applicative
import Control.Category hiding ((.))
import Foreign
import Prelude hiding (curry, fst, head, id, snd, scanl, tail, take, uncurry, (.))
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

{- |
Can be generated from output mapper, next-state function, and seed.

Laws:

@
'decons' ('unfold' o e s) f = f (o s) ('unfold' o e (e s))
@

@
'head' ('unfold' o e s) f = o s
@

@
'tail' ('unfold' o e s) f = 'unfold' o e (e s)
@
-}
class Unfold f where
    unfold :: (s -> a) -> (s -> s) -> s -> f a

instance Unfold [] where
    unfold o e =
        loop
        where
            loop s = o s : loop (e s)
    {-# INLINE unfold #-}

class Take f g where take :: Int -> f a -> g a

instance Take f (TakeF f) where take = MkTakeF
instance Take [] [] where take = P.take
instance (Point f) => Point (TakeF f) where point = MkTakeF 1 . point
instance (Functor f) => Functor (TakeF f) where fmap f (MkTakeF n x) = MkTakeF n (fmap f x)

instance (Applicative f) => Applicative (TakeF f) where
    pure = MkTakeF 1 . pure
    (MkTakeF m f) <*> (MkTakeF n x) = MkTakeF (min m n) (f <*> x)

data TakeF f a
    = MkTakeF Int (f a)

instance (Decons f) => Trans (TakeF f) [] where
    trans =
        loop
        where
            loop (MkTakeF 0 _) = []
            loop (MkTakeF n x) = decons x $ \ h t -> h : loop (MkTakeF (n - 1) t)

{- |
If you have 'Point' and 'Cons', or if you have 'Unfold',
then you get 'FromList' for free.

Laws:

@
'fromList' z = 'unfold' ('headWithDef' z) 'tailOrEmpty'
@

@
'fromList' z [] = 'point' z
@

@
'fromList' z (x:y) = 'cons' x ('fromList' z y)
@
-}
class FromList f where
    -- | The first argument is the value to be used if the list is empty.
    fromList :: a -> [a] -> f a

instance FromList [] where fromList = flip const

fromList_unfold :: (Unfold f) => a -> [a] -> f a
fromList_unfold z = unfold (headWithDef z) tailOrEmpty

headWithDef :: a -> [a] -> a
headWithDef x [] = x
headWithDef _ (x:_) = x

tailOrEmpty :: [a] -> [a]
tailOrEmpty [] = []
tailOrEmpty (_:x) = x

fromList_cons :: (Point f, Cons f) => a -> [a] -> f a
fromList_cons z =
    loop
    where
        loop lst =
            case lst of
                [] -> point z
                h:t -> cons h (loop t)

{- |
Laws:

If f satisfies 'Cons' and 'Decons',

@
'decons' ('cons' x y) f = f x y
@
-}
class Cons f where cons :: a -> f a -> f a

instance Cons [] where cons = (:)

{- |
If f is an instance of 'Decons',
its 'DeconsM' m instance is trivial.
See 'DeconsM'.
-}
class (Head f, Tail f) => Decons f where
    {- |
@
'decons' x f = f ('head' x) ('tail' x)
@
    -}
    decons :: f a -> (a -> f a -> b) -> b
    decons x f = f (head x) (tail x)
    {-# INLINE decons #-}

{- |
Law:

If f is already an instance of 'Decons',

@
deconsM = decons
@
-}
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
    {-# INLINE fill_ #-}

type Consumer a = Index Int -> a -> IO ()
type Index a = a
type Count a = a

{- |
If you have 'Cons' and 'Decons', you get 'Scan' for free.

@
@
-}
class Scan f where
    {- |
This generalizes the 'P.scanl' in "Prelude".

@
scanl (+) 0 \< 1, 2, 3, 4, ... \> = \< 0, 1, 3, 6, 10, ... \>
@
    -}
    scanl :: (a -> e -> a) -> a -> f e -> f a

instance Scan [] where scanl = P.scanl

scanl_cons :: (Cons f, Decons f) => (a -> e -> a) -> a -> f e -> f a
scanl_cons f =
    loop
    where
        loop a x =
            decons x $ \ h t ->
                cons a (loop (f a h) t)
{-# INLINE scanl_cons #-}

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
class Trans f g where trans :: f a -> g a

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

-- | Sample conversion.
class Convert a b where convert :: a -> b
instance Convert Double Int16 where convert = truncate . (32767 *)
instance (Convert a b, Functor f) => Convert (f a) (f b) where convert = fmap convert
