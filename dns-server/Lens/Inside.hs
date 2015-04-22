{-# LANGUAGE Safe #-}
module Lens.Inside
(
    Inside
    -- * Creation
    , mkWithModify
    , mkWithSet
    -- * Operations
    , get
    , set
    , modify
    -- * Infix operators
    , (^.)
    , (.>)
    , (=$)
    , (=<$>)
    , (=:)
)
where

import qualified Control.Category as C

{- |
An inhabitant of @Inside s b@ is a proof that an @s@ is in a @b@.

Laws:

@
get lens (modify lens f x) = f (get lens x)
@

@
get id = id
get (x . y) = get y . get x
@

@
modify id = id
modify (x . y) = modify x . modify y
@
-}
data Inside s b
    = MkInside
    {
        _get :: b -> s
        , _modify :: (s -> s) -> (b -> b)
    }

mkWithModify :: (b -> s) -> ((s -> s) -> (b -> b)) -> Inside s b
mkWithModify = MkInside

mkWithSet :: (b -> s) -> (s -> b -> b) -> Inside s b
mkWithSet g s = mkWithModify g (\ f b -> s (f $ g b) b)

instance C.Category Inside where
    id = mkWithModify id id
    (.) = flip compose

-- Left-to-right composition.
compose :: Inside s m -> Inside m b -> Inside s b
compose x0 x1 = mkWithModify g m
    where
        g0 = get x0
        g1 = get x1
        g = g0 . g1
        m0 = modify x0 -- (s -> s) -> (m -> m)
        m1 = modify x1 -- (m -> m) -> (b -> b)
        m = m1 . m0 -- (s -> s) -> (b -> b)

-- | Project the small value out of the big value.
get :: Inside s b -> b -> s
get = _get

-- | Replace the small value in the big value.
set :: Inside s b -> s -> b -> b
set lens repl = modify lens (const repl)

-- | Get, apply, and set.
modify :: Inside s b -> (s -> s) -> b -> b
modify = _modify

-- | Infix @'flip' 'get'@. Left-associate. Precedence 2.
(^.) :: b -> Inside s b -> s
(^.) = flip get
infixl 2 ^.

-- | Procedural-style composition. Right-associate. Precedence 9. The same as 'C..'.
(.>) :: Inside m b -> Inside s m -> Inside s b
(.>) = flip compose
infixr 9 .>

-- | Infix 'modify'. Right-associate. Precedence 2.
(=$) :: Inside s b -> (s -> s) -> (b -> b)
(=$) = modify
infixr 2 =$

-- | Infix 'modify' whose containee type is a 'Functor'-ed type. Right-associate. Precedence 2.
(=<$>) :: (Functor f) => Inside (f s) b -> (s -> s) -> (b -> b)
inside =<$> endo = inside =$ fmap endo
infixr 2 =<$>

-- | Infix 'set'. Right-associate. Precedence 2.
(=:) :: Inside s b -> s -> (b -> b)
(=:) = set
infixr 2 =:
