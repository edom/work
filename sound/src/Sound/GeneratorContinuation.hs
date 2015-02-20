{-# LANGUAGE BangPatterns #-}

{- |
Continuation-based generator.
-}
module Sound.GeneratorContinuation
(
    gcIterate
    , gcIterateSimple
)
where

import Control.Monad.Trans.Cont

{- |
A potentially infinite stream.

The consumer is called with each element.

@
'callCC' '$' \\ break ->
    gcIterateSimple (0 :: 'Int') (1 '+') '$' \\ v -> do
        'when' (v '>=' 1000) '$' break '()'
        'liftIO' '$' 'print' v
@
-}
gcIterate
    :: (Monad m)
    => a -- ^ initial value
    -> (a -> ContT r m a) -- ^ next-value function
    -> (a -> ContT r m b) -- ^ consumer
    -> ContT r m c

gcIterate init_ next yield =
    let
        loop !cur =
            yield cur
            >> next cur
            >>= loop
    in
        loop init_

gcIterateSimple
    :: (Monad m)
    => a -- ^ initial value
    -> (a -> a) -- ^ next-value function
    -> (a -> ContT r m b) -- ^ consumer
    -> ContT r m c

gcIterateSimple init_ next yield =
    let
        loop !cur =
            yield cur
            >> loop (next cur)
    in
        loop init_
