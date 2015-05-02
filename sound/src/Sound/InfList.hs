{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sound.InfList
(
    -- * Construction
    L(..)
    , SL
    , RL
    , SRL
    , lat
    , lcons
    , ldelay
    , ldelays
    , lcycle
    , literate
    , lgenerate
    , lnat
    -- * Scans
    , lscanl
    , lunfoldr
    , lzip3
    , lcmap
    , lcid
    , lczip
    , lcfoldl
    , lfoldl
    , lcfoldr
    , lcfoldlM
    -- * Inconsistently named continuation-based transformation
    , ltakezipmap
    , Main
    , Side
    , ltakemap
    , ltakeadd
    , ltakeappend
    , ltakewhilemap
    , ltakewhileappend
    -- * Substreams
    , ldrop
    , ldropwhile
    , lfromlist
    , listfroml
    , ltakelist
    -- * Monad
    , lmapM_
    , lmapMc_
    , limapM_
    , ltakemapM_
    -- * Tracker
    , lfromindexes
    , lgeos
    , Slice(..)
    , slice
    , smap
    , slmapM_
    , srlmapM_
    , rlmap
    , rlrepeat
    , rltake
    , rltakes
    , ltime
    , rltime
    , rldropwhile
    , listfromrl
    , rltakezipmap
    , rltakezipmaps
    , rltakemap
    , rltakeadd
    , rltakeadds
    , rltakeappends
)
where

import Sound.Class
import Sound.Time

{- |
This is similar to list, but this is strict on the head
and this does not have a zero-argument constructor.

Lists can be infinite but streams must be infinite.

@
data List a = Nil
            | Cons a (List a)

data L a    = MkL !a (L a)
@
-}
data L a
    = MkL !a (L a)
    deriving (Read, Show)

-- | This is similar to the 'Functor' instance of '[]'.
instance Functor L where fmap = lmap
instance Point L where point = lrepeat
instance Zip2 L where zip2 = lzip2
instance Head L where head (MkL x _) = x
instance Tail L where tail (MkL _ x) = x
instance Cons L where cons = MkL
instance Decons L
instance DeconsM m L where deconsM = decons
instance Consume L
instance Fill L
instance HardSync L

-- | This is similar to the 'Applicative' instance of 'ZipList'.
instance Applicative L where
    pure = point
    (<*>) = zip2 ($)

-- | This allows treating streams of numbers as numbers. Arithmetic operations are done elementwise.
instance (Num a) => Num (L a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = lrepeat . fromInteger

-- | This allows elementwise division of streams of numbers.
instance (Fractional a) => Fractional (L a) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = lrepeat . fromRational

-- | This allows applying transcendental functions to streams of numbers.
instance (Floating a) => Floating (L a) where
    pi = lrepeat pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    atan = fmap atan
    acos = fmap acos
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

{- |
@
lzip2 f \< x0, x1, ... \> \< y0, y1, ... \> = \< f x0 y0, f x1 y1, ... \>
@
-}
lzip2 :: (a -> b -> c) -> L a -> L b -> L c
lzip2 f =
    loop
    where
        loop (MkL xh xt) (MkL yh yt) =
            MkL (f xh yh) (loop xt yt)
{-# INLINE lzip2 #-}

lat :: Int -> L a -> a
lat = loop
    where
        loop !n (MkL h t) =
            if n <= 0
                then h
                else loop n t
{-# INLINE lat #-}

{- |
Add an element to the front of the stream.

@
lcons a \< x0, x1, x2, ... \> = \< a, x0, x1, x2, ... \>
@
-}
lcons :: a -> L a -> L a
lcons x = MkL x
infixr 5 `lcons`

{- |
Unit delay.

@
ldelay = 'lcons' 0
@

@
ldelay \< x0, x1, x2, ... \> = \< 0, x0, x1, x2, ... \>
@
-}
ldelay :: L Double -> L Double
ldelay = lcons 0

{- |
Non-unit delay. This should be faster than composing lots of 'ldelay's.

The first argument is the number of zeros to be added to the front of the stream.
-}
ldelays :: Int -> L Double -> L Double
ldelays n = ltakeappend n (lrepeat 0)

{- |
A stream whose all elements are the same.

@
lrepeat = 'literate' id
@

@
lrepeat x = \< x, x, x, ... \>
@
-}
lrepeat :: a -> L a
lrepeat !x = loop
    where
        loop = MkL x loop
{-# INLINE lrepeat #-}

lcycle :: Int -> L a -> L a
lcycle cylen strm =
    loop
    where
        loop = ltakeappend cylen strm loop

{- |
Apply a function repeatedly to an element.

@
literate f x = \< x, f x, f (f x), ... \>
@
-}
literate :: (a -> a) -> a -> L a
literate !f =
    loop
    where
        loop !v =
            let
                !w = f v
                t = loop w
            in
                MkL v t
{-# INLINE literate #-}

{- |
Map a stream of natural numbers sorted ascending.

@
lgenerate f = 'fmap' f ('literate' 'succ' 0)
@

@
lgenerate f = \< f 0, f 1, f 2, ... \>
@
-}
lgenerate :: (Int -> a) -> L a
lgenerate f = lunfoldr (1 +) f 0

{- |
Stream of natural numbers sorted ascending.

@
lnat = \< 0, 1, 2, ... \>
@
-}
lnat :: (Num a) => L a
lnat = literate (1 +) 0

{- |
@
lscanl (+) 0 \< 1, 2, 3, 4, ... \> = \< 0, 1, 3, 6, 10, ... \>
@
-}
lscanl :: (a -> b -> a) -> a -> L b -> L a
lscanl f =
    loop
    where
        loop acc (MkL h t) =
            MkL acc (loop (f acc h) t)

-- ** Unfolding

{- |
@
lunfoldr fs fa s = 'fmap' fa ('literate' fs s)
@

@
lunfoldr fs fa s = \< fa s, fa (fs s), fa (fs (fs s)), ... \>
@
-}
lunfoldr :: (s -> s) -> (s -> a) -> s -> L a
lunfoldr !fs !fa =
    loop
    where
        loop !s =
            let
                !a = fa s
                !t = fs s
                u = loop t
            in
                MkL a u
{-# INLINE lunfoldr #-}

-- * Deconstruction

-- * Transformation

{- |
Apply the function to each element of the stream.

@
lmap f \< x0, x1, x2, ... \> = \< f x0, f x1, f x2, ... \>
@
-}
lmap :: (a -> b) -> L a -> L b
lmap f =
    loop
    where
        loop (MkL h t) = MkL (f h) (loop t)
{-# INLINE lmap #-}

{- |
@
lzip3 f \< x0, x1, ... \> \< y0, y1, ... \> \< z0, z1, ... \> = \< f x0 y0 z0, f x1 y1 z1, ... \>
@
-}
lzip3 :: (a -> b -> c -> d) -> L a -> L b -> L c -> L d
lzip3 f =
    loop
    where
        loop (MkL xh xt) (MkL yh yt) (MkL zh zt) =
            MkL (f xh yh zh) (loop xt yt zt)
{-# INLINE lzip3 #-}

-- * Continuation-based transformation

{- $

The functions in this section follow this pattern:

@
lcfff n ... x0 x1 ... c ~ 'take' n (fff x0 x1 ...) '++' c ('drop' n x0) ('drop' n x1) ...
@

The first argument, which can be a number or a predicate,
determines the length of the prefix that is being manipulated.

The last argument is the continuation.

-}

-- ** Mapping

{- |
@
lcmap n f x c ~ 'take' n ('map' f x) '++' c ('drop' n x)
@
-}
lcmap :: Int -> (a -> b) -> L a -> (L a -> L b) -> L b
lcmap n_ f_ x_ c_ =
    loop n_ x_
    where
        loop n (MkL h t) | n > 0 = MkL (f_ h) (loop (n - 1) t)
        loop _ x = c_ x

{- |
@
lcid n x c = 'lcmap' n 'id' x c
lcid n x c ~ 'take' n x '++' c ('drop' n x)
@
-}
lcid :: Int -> L a -> (L a -> L a) -> L a
lcid n_ x_ c_ =
    loop n_ x_
    where
        loop n (MkL h t) | n > 0 = MkL h (loop (n - 1) t)
        loop _ x = c_ x

{- |
Mix two streams.

@
lczip n f x y c ~ 'take' n ('zipWith' f x y) '++' c ('drop' n x) ('drop' n y)
@
-}
lczip :: Int -> (a -> b -> c) -> L a -> L b -> (L a -> L b -> L c) -> L c
lczip n_ f_ x_ y_ c_ =
    loop n_ x_ y_
    where
        loop n (MkL xh xt) (MkL yh yt) | n > 0 = MkL (f_ xh yh) (loop (n - 1) xt yt)
        loop _ x y = c_ x y

-- ** Folding

{- |
@
lcfoldl n r a x c ~ c (foldl r a (take n x)) (drop n x)
@
-}
lcfoldl
    :: Int              -- ^ length of prefix to be folded
    -> (a -> e -> a)    -- ^ reducing function
    -> a                -- ^ initial accumulator value
    -> L e              -- ^ stream whose prefix is to be folded
    -> (a -> L e -> r)  -- ^ continuation
    -> r
lcfoldl n_ r a_ x_ c =
    loop n_ a_ x_
    where
        loop n a x@(MkL h t) =
            if n <= 0
                then c a x
                else loop (n - 1) (r a h) t
{-# INLINE lcfoldl #-}

lfoldl
    :: Int              -- ^ length of prefix to be folded
    -> (a -> e -> a)    -- ^ reducing function
    -> a                -- ^ initial accumulator value
    -> L e              -- ^ stream whose prefix is to be folded
    -> a
lfoldl n_ !r a_ x_ =
    loop n_ a_ x_
    where
        loop !n !a (MkL h t) =
            if n <= 0
                then a
                else loop (n - 1) (r a h) t
{-# INLINE lfoldl #-}

{- |
@
lcfoldr n r a x c ~ c (foldr r a (take n x)) (drop n x)
@
-}
lcfoldr
    :: Int              -- ^ length of prefix to be folded
    -> (e -> a -> a)    -- ^ reducing function
    -> a                -- ^ initial accumulator value
    -> L e              -- ^ stream whose prefix is to be folded
    -> (a -> L e -> r)  -- ^ continuation
    -> r
lcfoldr n_ r a_ x_ c =
    loop n_ a_ x_
    where
        loop n a x@(MkL h t) =
            if n <= 0
                then c a x
                else loop (n - 1) (r h a) t
{-# INLINE lcfoldr #-}

{- |

-}
lcfoldlM
    :: (Monad m)
    => Int                  -- ^ length of prefix to be folded
    -> (a -> e -> m a)      -- ^ reducing function
    -> a                    -- ^ initial accumulator value
    -> L e                  -- ^ stream whose prefix is to be folded
    -> (a -> L e -> m r)    -- ^ continuation
    -> m r
lcfoldlM n_ r a_ x_ c =
    loop n_ a_ x_
    where
        loop n a x@(MkL h t) =
            if n <= 0
                then c a x
                else r a h >>= \ b -> loop (n - 1) b t
{-# INLINE lcfoldlM #-}

{- |
This mixes a finite prefix of the side stream @y@
elementwise into the main stream @x@.

The rest of the side stream is forgotten.

@
ltakezipmap f n x y c ~ 'take' n ('zipWith' f x y) '++' c ('drop' n x)
@
-}
ltakezipmap
    :: (Main a -> Side a -> a)  -- ^ elementwise combining function; first argument is main stream sample; second argument is side stream sample
    -> Int                  -- ^ prefix length (number of elements to mix)
    -> Main (L a)           -- ^ main stream
    -> Side (L a)           -- ^ side stream
    -> (Main (L a) -> L a)  -- ^ main stream continuation
    -> L a                  -- ^ result
ltakezipmap z_ n_ x_ y_ c_ =
    loop n_ x_ y_
    where
        loop n (MkL xh xt) (MkL yh yt) | n >= 0 = MkL (z_ xh yh) (loop (n - 1) xt yt)
        loop _ x _ = c_ x

-- | The focus of the operation.
type Main a = a

-- | Something incorporated to a 'Main' and then forgotten.
type Side a = a

{- |
@
ltakemap n x c ~ 'take' n x '++' c ('drop' n x)
@

but @ltakemap@ would go through the stream only once.

The function argument is what you want to do with the rest of the stream.
We can think of this function as a continuation.
-}
ltakemap :: Int -> L a -> (L a -> L a) -> L a
ltakemap n_ x_ f_ =
    loop n_ x_
    where
        loop n (MkL h t) | n > 0 = MkL h (loop (n - 1) t)
        loop _ x = f_ x

{- |
@
ltakeadd n x y = 'ltakemap' n x ('zip2' ('+') y)
@
-}
ltakeadd :: (Num a) => Int -> L a -> L a -> L a
ltakeadd n x y = ltakemap n x (zip2 (+) y)

{- |
@ltakeappend n x y@ is a stream
whose first @n@ elements are the first @n@ elements of @x@,
and whose other elements are taken from @y@.

@
ltakeappend n x y = \< x0, x1, ..., x(n - 1), y 0, y1, ... \>
ltakeappend n x y ~ 'take' n x '++' y
@

You can concatenate many streams like this:

@
ltakeappend n0 x0 '.' ltakeappend n1 x1 '.' ltakeappend n2 x2 '.' ... '.' ltakeappend nm xm rest
@

That also holds for variants of ltakeappend.

We can state @ltakeappend@ in terms of 'ltakemap':

@
ltakeappend n x y = 'ltakemap' n x ('const' y)
@
-}
ltakeappend :: Int -> L a -> L a -> L a
ltakeappend count first second = ltakemap count first (const second)

{- |
@
ltakewhilemap p x c ~ 'takeWhile' p x '++' c ('dropWhile' p x)
@
-}
ltakewhilemap :: (a -> Bool) -> L a -> (L a -> L a) -> L a
ltakewhilemap p_ x_ c_ =
    loop x_
    where
        loop (MkL h t) | p_ h = MkL h (loop t)
        loop x = c_ x

{- |
@
ltakewhileappend p x y ~ 'takeWhile' p x '++' y
@
-}
ltakewhileappend :: (a -> Bool) -> L a -> L a -> L a
ltakewhileappend cond stm newTail = ltakewhilemap cond stm (const newTail)

-- | Discard the first @n@ elements of the stream.
ldrop :: Int -> L a -> L a
ldrop =
    loop
    where
        loop n x | n > 0 = loop (n - 1) (tail x)
        loop _ x = x

-- | Discard all first elements of the stream that satisfy the predicate.
ldropwhile :: (a -> Bool) -> L a -> L a
ldropwhile cond =
    loop
    where
        loop s@(MkL h t) =
            if cond h
                then loop t
                else s

-- * List conversion

-- | Extend the list infinitely with the given element.
lfromlist :: a -> [a] -> L a
lfromlist z =
    loop
    where
        loop lst =
            case lst of
                [] -> lrepeat z
                h : t -> MkL h (loop t)

{- |
Turn a stream into an infinite list.
-}
listfroml :: L a -> [a]
listfroml (MkL h t) =
    h : listfroml t

{- |
Extract the first @n@ elements of a stream, where @n@ is the first argument.

@
ltakelist n x = 'take' n ('listfroml' x)
@
-}
ltakelist :: Int -> L a -> [a]
ltakelist n = take n . listfroml

lmapM_ :: (Monad m) => (a -> m b) -> Int -> L a -> m ()
lmapM_ k n_ x_ =
    loop n_ x_
    where
        loop !n (MkL h t) =
            if n <= 0
                then return ()
                else k h >> loop (pred n) t

lmapMc_ :: (Monad m) => (a -> m b) -> Int -> L a -> (L a -> m ()) -> m ()
lmapMc_ k n_ x_ c =
    loop n_ x_
    where
        loop n x@(MkL h t) =
            if n <= 0
                then c x
                else k h >> loop (pred n) t

limapM_ :: (Monad m) => (Int -> a -> m b) -> Int -> L a -> m ()
limapM_ k n_ x_ =
    loop 0 x_
    where
        loop !i (MkL h t) =
            if i >= n_
                then return ()
                else k i h >> loop (i + 1) t
{-# INLINE limapM_ #-}

{- |
This is similar to 'Control.Monad.mapM_'.

@
ltakemapM_ k n \< x0, x1, ... \> = k x0 '>>' k x1 >> ... >> k x(n-1)
@
-}
ltakemapM_ :: (Monad m) => (a -> m b) -> Int -> L a -> m ()
ltakemapM_ k count x@(MkL !_ _) =
    loop 0 x
    where
        loop n (MkL !h t) =
            if n >= count
                then return ()
                else
                    k h >> loop (n + 1) t
{-# INLINE ltakemapM_ #-}

{- |
The input must be monotonically increasing.

@
lfromindexes \< x0, x1, x2, ... \> = \< y0, y1, y2, ... \>
where
y(x0) = True
y(x1) = True
y(x2) = True
...
yn = False for each other n
@

If @x@ is monotonically increasing, then @lfromindexes x = y@
where @yn@ (the element of @y@ at index @n@) is true if and only if @n@ is an element of @x@.
-}
lfromindexes :: L Int -> L Bool
lfromindexes xs_ =
    loop 0 xs_
    where
        loop k x@(MkL h t) =
            if k == h
                then MkL True (loop (succ k) t)
                else MkL False (loop (succ k) x)

{- |
Geometric series.

The same as sampling an exponential ramp.

@
lgeos a r = \< a, r * a, r * r * a, r * r * r * a, ... \>
@
-}
lgeos :: (Num a) => Init a -> Ratio a -> L a
lgeos init_ ratio = literate (ratio *) init_

type RL a = Rated (L a)

type SL b a = Slice b (L a)

type SRL b a = Slice b (Rated (L a))

{- |
A finite countable contiguous part of something.
The first type parameter is
the type of the boundary.
-}
data Slice b a
    = MkSlice
    {
        _slcount :: b
        , _unslice :: a
    }

instance Functor (Slice n) where
    fmap = smap

{- |
If @n@ is an @'Int'@ then @slice n x@ represents the first @n@ elements of @x@.
-}
slice :: Nonnegative b -> a -> Slice b a
slice = MkSlice

smap :: (a -> b) -> Slice n a -> Slice n b
smap f s = s { _unslice = f (_unslice s) }

slmapM_ :: (Monad m) => (a -> m b) -> Slice Int (L a) -> m ()
slmapM_ k x = ltakemapM_ k (_slcount x) (_unslice x)

srlmapM_ :: (Monad m) => (a -> m b) -> Slice Int (Rated (L a)) -> m ()
srlmapM_ k x = ltakemapM_ k (_slcount x) (_unrated $ _unslice x)

rlmap :: (a -> b) -> Rated (L a) -> Rated (L b)
rlmap f = rmap (lmap f)

rlrepeat :: a -> RL a
rlrepeat = rated (mkRate 0) . lrepeat

{- |
@
rltake = 'slice'
@
-}
rltake :: Int -> RL a -> SRL Int a
rltake = slice

rltakes :: Second Double -> RL a -> SRL Int a
rltakes t x =
    rltake n x
    where
        r = _unRate $ rate x
        n = round $ fromIntegral r * t

ltime :: Rate Int -> (L Double)
ltime r = lunfoldr fs fa s
    where
        fs = (1 +)
        fa i = fromIntegral i * dt
        s = 0 :: Int
        dt = recip . fromIntegral . _unRate $ r

rltime :: Rate Int -> Rated (L Double)
rltime r = rated r (ltime r)

rldropwhile :: (a -> Bool) -> RL a -> RL a
rldropwhile cond = rmap (ldropwhile cond)

listfromrl :: RL a -> [a]
listfromrl = listfroml . unrated

rltakezipmap :: (a -> a -> a) -> Int -> RL a -> RL a -> (RL a -> RL a) -> RL a
rltakezipmap z n x y c =
    rated r (ltakezipmap z n (unrated x) (unrated y) (unrated . c . rated r))
    where
        r = rate x `ror` rate y

rltakezipmaps :: (a -> a -> a) -> T -> RL a -> RL a -> (RL a -> RL a) -> RL a
rltakezipmaps z t x y =
    rltakezipmap z n x y
    where
        r = rate x `ror` rate y
        n = round (t * fromIntegral (_unRate r))

rltakemap :: Int -> RL a -> (RL a -> RL a) -> RL a
rltakemap n x_ f =
    rmap (\ x -> ltakemap n x (unrated . f . rated r)) x_
    where
        r = rate x_

rltakeadd :: (Num a) => Int -> RL a -> RL a -> RL a
rltakeadd n = rlift2 (ltakeadd n)

rltakeadds :: (Num a) => T -> RL a -> RL a -> RL a
rltakeadds s x y = rltakeadd n x y
    where
        r = rate x `ror` rate y
        n = round (s * fromIntegral (_unRate r))

-- | This is like 'ltakeappend' but this has the duration in seconds instead of sample count.
rltakeappends :: T -> RL a -> RL a -> RL a
rltakeappends time first second =
    MkRated r x
    where
        n = round (fromIntegral (_unRate r) * time)
        r = rate first `ror` rate second
        x = ltakeappend n (unrated first) (unrated second)
