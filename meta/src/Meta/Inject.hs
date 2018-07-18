{-
2018-07-18

This tries to use type classes to generalize pure, map, and bind to arbitrary layers of Monads.

Tested with GHC 8.2.

https://github.com/edom/work/tree/master/meta/src/Meta/Inject.hs
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Meta.Inject where

import Control.Monad (join)



class PureR a b where

    pureR :: a -> b

instance {-# OVERLAPPABLE #-} (a ~ b) => PureR a b where
    pureR = id

instance {-# OVERLAPPING #-} (Applicative m, PureR a b) => PureR a (m b) where
    pureR = pure . pureR






class MapR a b c d where
    mapR :: (a -> b) -> (c -> d)

instance {-# OVERLAPPABLE #-} (a ~ c, b ~ d) => MapR a b c d where
    mapR f = f

-- If we pick this, then @bindR@ works, but @mapR inc (pureR i) :: Maybe (Maybe Char)@ breaks.
instance {-# OVERLAPPING #-} (Functor m, MapR a b c d, mc ~ m c, m d ~ md) => MapR a b (m c) md where
    mapR f = fmap (mapR f)

{-
-- If we pick this, then @bindR@ breaks, but @mapR inc (pureR i) :: Maybe (Maybe Char)@ works.
instance {-# OVERLAPPING #-} (Functor m, MapR a b c d, mc ~ m c, m d ~ md) => MapR a b mc (m d) where
    mapR f = fmap (mapR f)
-}






class JoinR m a b where
    joinR :: m a -> m b

instance {-# OVERLAPPABLE #-} (Monad m, a ~ b) => JoinR m a b where
    joinR = id

instance {-# OVERLAPPING #-} (Monad m, JoinR m a b) => JoinR m (m a) b where
    joinR = joinR . join




i :: Char
i = 'A'

mi :: Maybe Char
mi = pureR i

mmi :: Maybe (Maybe Char)
mmi = pureR i

mmmi :: Maybe (Maybe (Maybe Char))
mmmi = pureR i

ni :: Maybe (Either String (Maybe [Maybe Char]))
ni = pureR i

inc :: Char -> Char
inc a = toEnum (fromEnum a + 1)

ka :: Char -> Maybe Char
ka 'A' = return 'A'
ka _ = fail "not A"

minc :: Maybe (Char -> Char)
minc = return inc

-- (Control.Monad.>>=).

-- This requires AllowAmbiguousTypes.
-- GHC infers the following type:
-- bindR :: (MapR a2 b2 c (m a1), JoinR m a1 b1) => c -> (a2 -> b2) -> m b1
-- but if we put that signature in this source file, the program fails to compile.
-- This @bindR@ somehow works.

-- To see this in action, try @bindR A B@ with @A in { mi, mmi, mmmi }@ and @B in { inc, ka }@.
-- Also, surprisingly @bindR i ka@ returns a @Maybe Char@.

-- Compare to Monad law:
-- bind m k = join (map k m)

bindR m k = joinR (mapR k m)



-- (=<<)
extR k m = joinR (mapR k m)



-- Things that don't work.



-- Control.Monad.ap.
-- This compiles, but @apR minc mi@ doesn't work.
-- However, it works if @pureR@ is replaced by Just, or even (Just . Just).

apR mf mx =
    bindR mf $ \ f ->
    bindR mx $ \ x ->
    pureR (f x)



-- This @minc@ doesn't work. See AppR.

-- minc :: Maybe (Char -> Char)
-- minc = pureR inc



-- It seems that this doesn't work if there is any non-unary type constructor somewhere (such as (->)).
class AppR f a b where
    appR :: f -> a -> b

instance {-# OVERLAPPABLE #-} (f ~ (a -> b)) => AppR f a b where
    appR f a = f a

instance {-# OVERLAPPING #-} (Applicative m, AppR f a b, m b ~ mb) => AppR (m f) (m a) mb where
    appR mf ma = (\ f a -> appR f a) <$> mf <*> ma
