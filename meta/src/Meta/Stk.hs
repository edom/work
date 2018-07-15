{- |
Stack machine.

This was made with the JVM in mind.

We can use continuation-passing style to translate a stack machine subroutine to a lambda calculus expression.

A stack instruction @ins@ is translated into something like @translate ins : args -> stack0 -> (stack1 -> res -> end) -> end@.

For example, the following sequence:

@
push 1
push 2
add
@

translates to the following expression:

@
\ s0 cont ->
    push 1 s0 (\ s1 _ ->
        push 2 s1 (\ s2 _ ->
            add (a,b) s2 cont
        )
    )
@

and then we rely on the simplifier to turn that mess into @1 + 2@.

Concatenating two sequences should compose the expression.

@
seq1; seq2 ==> translate seq1 (\ s cont -> translate seq2 s cont)
@
-}
module Meta.Stk (
    Prog
    , Ins(..)
) where

type Prog v = [Ins v]

{- |
An instruction.

The parameter @v@ is the value type.
-}
data Ins v
    = Push v
    | Pop
    | Add
    deriving (Read, Show)
