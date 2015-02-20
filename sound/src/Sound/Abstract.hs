{-# LANGUAGE BangPatterns #-}
{- |
Abstraction ladders.
-}
module Sound.Abstract
(
    module Sound.Pair
    , Previous
    -- * Fast Bool
    , IntBool
    , ibFalse
    , ibTrue
    , ibIf
    , ibFromBool
    -- * Loop body
    , Endo
    , EndoM
    , Kleisli
    , Kleisli2
    , GenBody(..)
    , GenBodyM(..)
    -- * Stream
    -- $stream
    , G(..)
    , Gm(..)
    , gmap
    , gmFromG
)
where

import Control.Applicative

import Sound.Pair

type Previous a = a

type IntBool = Int

ibIf :: IntBool -> a -> a -> a
ibIf !c !t !f =
    case c of
        0 -> f
        _ -> t
{-# INLINE ibIf #-}

ibFalse :: IntBool
ibFalse = 0
{-# INLINE ibFalse #-}

ibTrue :: IntBool
ibTrue = 1
{-# INLINE ibTrue #-}

ibFromBool :: Bool -> IntBool
ibFromBool x =
    case x of
        False -> 0
        True -> 1
{-# INLINE ibFromBool #-}

{- |
An @Endo a@ is an endofunction of @a@ (a function from @a@ to @a@).
Endofunctions describe loop bodies that GHC can optimize.
(GHC cannot optimize a recursive definition that crosses a constructor.)

Applying an input to the endofunction is executing the loop body once.

Composing two endofunctions is concatenating the loop bodies.

This generalizes to 'EndoM'.
-}
type Endo a = a -> a

{- |
This specializes to 'Endo'.

@
'Endo' a ~ 'EndoM' 'Identity' a
@

This generalizes to 'Kleisli'.
-}
type EndoM m a = a -> m a

{- |
Kleisli arrow.

This specializes to 'EndoM'.

@
'EndoM' m a ~ 'Kleisli' m a a
@
-}
type Kleisli m i o = i -> m o

{- |
@
'Kleisli2' m i j o ~ 'Kleisli' m ('P' i j) o
@
-}
type Kleisli2 m i j o = i -> j -> m o

{- |
This generalizes to 'GenBodyM'.
-}
data GenBody s a
    = MkGenBody
    {
        _gbe :: s -> s
        , _gbo :: s -> a
    }

data GenBodyM s m a
    = MkGenBodyM (s -> m s) (s -> m a)

{- $stream
A stream is a state and a loop body.
-}

{- |
A generator is described by a length,
an endofunction,
a starting value,
and an output mapper.

@G s a@ is similar to infinite @[a]@.

@
MkG s e o ~ [o s, o (e s), o (e (e s)), ...]
@

This generalizes to 'Gm'.

@
'G' s a ~ 'Gm' s 'Identity' a
@
-}
data G s a
    = MkG
    {
        _gs :: !s
        , _ge :: !(Endo s)
        , _go :: !(s -> a)
    }

instance Functor (G s) where
    fmap = gmap

gmap :: (a -> b) -> G s a -> G s b
gmap !f !x
    = x { _go = g }
    where
        !g = f . _go x
        {-# INLINE g #-}
{-# INLINE gmap #-}

{- |
A generator describes a stream that GHC 7.6.3 can optimize well with default optimization flags (just @-O@).

Type parameters:

[@s@] Generator state type.

[@m@] Usually 'IO' or 'Identity'.

[@i@] Input type.

[@o@] Type of element coming out of generator.

This specializes to 'G'.

@
'G' s a ~ 'Gm' s 'Identity' a
@

-}
data Gm s m a
    = MkGm
    {
        -- | Current state.
        _gms :: !s
{- |
State transition function.

To make GHC emit the fastest code,
evaluating this function shall /normalize/ its argument.
For @s@ that is a nested data type such as @'P' a ('P' b c)@,
evaluating the argument to /weak head-normal form/ using @seq@ does not suffice;
it has to be /normal form/.

If you do not use 'MkGm' directly, this property should be satisfied.
-}
        , _gme :: !(s -> m s)
        -- | Output mapper.
        , _gmo :: !(s -> m a)
    }

{- |
Generalize 'G' to 'Gm'.

Not strict enough?
-}
gmFromG :: (Applicative m) => G s a -> Gm s m a
gmFromG (MkG s e o) =
    MkGm s e' o'
    where
        e' !t = pure $! e t
        o' !t = pure $! o t
{-# INLINE gmFromG #-}
