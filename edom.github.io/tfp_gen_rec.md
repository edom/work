---
title: Approximating general recursion in TFP
date: 2018-04-07 00:00 +0700
permalink: /tfp_gen_rec.html
mathjax: yes
---

Here we are going to show how to approximate general recursive functions in TFP
with the help of the following `repeat` and `approx`:

```haskell
-- The expression `repeat n f x` means
-- `n` times of the application of `f` to `x`.
repeat : Nat -> (a -> a) -> a -> a
repeat 0 f = id
repeat (n+1) f = repeat n f . f

approx
    : Nat -- count
    -> (a -> s) -- begin
    -> (s -> s) -- step
    -> (s -> Maybe b) -- end
    -> (a -> Maybe b)

approx count begin step end =
    end . repeat count step . begin
```

To approximate a general recursive function `f : a -> b`,
we write a data type `S_f` and these three non-recursive total functions:

```haskell
begin_f : a -> S_f
step_f : S_f -> S_f
end_f : S_f -> Maybe b

-- A side note:
-- In PFP, the original `f` can be
-- recovered from those three functions:
f input = loop (begin_f input)
    where
        loop s = case end_f s of
            Just output -> output
            _ -> loop (step_f s)
```

Then, we can approximate `f` as `f_approx`:

```haskell
f_approx : Nat -> (a -> Maybe b)
f_approx count =
    approx count begin_f step_f end_f
```

The `count` parameter can be thought as a time limit or time-out,
the number of iterations,
the number of steps.

Here is an example approximation of the factorial function.

```haskell
fac 0 = 1
fac n = n * fac (n-1)

data State = Mk { n : Nat, a : Nat }

fac_approx count =
    end . repeat count step . begin
    where
        begin : Nat -> State
        begin n = Mk n 1

        end : State -> Maybe Nat
        end (Mk 0 a) = Just a
        end _ = Nothing

        step : State -> State
        step (Mk 0 a) = Mk 0 a
        step (Mk (n+1) a) = Mk n (a * (n+1))
```

Here is an example approximation of bottom.

```haskell
-- PFP

hang : a
hang = hang

-- TFP approximation

data State = Mk

hang_begin _ = Mk
hang_step s = s
hang_end _ = Nothing

hang_approx count =
    hang_end . repeat count hang_step . hang_begin
```

I conjecture that there is an algorithm that can transform every general recursive function into its begin-step-end form.
