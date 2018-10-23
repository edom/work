:- module(list, [
    replicate/3
]).

:- use_module(library(clpfd)).

/*
replicate(?Count, ?Elem, ?List)
is true iff List has length Count and every element of List is the same Elem.

Thanks to the clpfd library, all parameters are bidirectional.

The intended usage of this predicate is to generate a long list for testing.
Example: replicate(10, atom, List) generates a list of 10 atoms.

This is similar to the 'replicate' function in Haskell Prelude library Data.List module.
*/
replicate(0, _, []).
replicate(Count, Elem, [Elem|Tail]) :-
    Count #>= 0,
    Count_1 #= Count - 1,
    replicate(Count_1, Elem, Tail).
