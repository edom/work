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
)
where

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
