{-
Summary:

Let introduces sharing (thunk).

Lambda prevents sharing (unless in the case where ghc can float out the let).

The expression 'unsafePerformIO x' smuggles the side effect of x into beta-reduction.
The side effect of 'x' is performed when the 'unsafePerformIO x' expression is beta-reduced.
A thunk will be reduced at most once.
Thus the side effect will happen at most once.
If the thunk is shared, it will not be reduced more than once.

How to run:

Try this in ghci:

:l LetLambdaNormal.hs

always ()
once ()
always ()
once ()
-}

module LetLambdaNormal
where

import System.IO.Unsafe (unsafePerformIO)

-- This prints "unsafe" every time you ask ghci to evaluate 'always ()'.
-- The let is inside the lambda.
always :: a -> ()
always =
    \ x ->
        let
            side = unsafePerformIO (putStrLn "unsafe")
        in
            side

-- This prints "unsafe" only the first time you ask ghci to evaluate 'once ()'.
-- The let is outside the lambda.
once :: a -> ()
once =
    let
        side = unsafePerformIO (putStrLn "unsafe")
    in
        \ x -> side

-- Similar to the case in lambda calculus,
-- in Haskell we can also rewrite 'let x = y in z' to '(\ x -> z) y'
stillAlways :: a -> ()
stillAlways =
    \ x -> (\ side -> side) (unsafePerformIO (putStrLn "unsafe"))

stillOnce :: a -> ()
stillOnce =
    (\ side -> \ x -> side) (unsafePerformIO (putStrLn "unsafe"))

-- This prints "1\n2\n" as a side effect, which demonstrates that Haskell evaluates expression in normal order.
normal :: a -> Int
normal _dontShare =
    (+)
        (unsafePerformIO (putStrLn "1" >> return 1))
        (unsafePerformIO (putStrLn "2" >> return 2))
