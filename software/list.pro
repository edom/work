:- module(list, [
    replicate/3
    , repeat_thing_list/3
]).

:- use_module(library(clpfd)).

/** replicate(?Count, ?Elem, ?List)

"List has length Count and every element of List is the same Elem."

Every parameter is bidirectional.

The intended usage of this predicate is to generate a long list for testing.
Example: replicate(10, x, List) generates a list of 10 =x=s.

This is similar to the 'replicate' function in Haskell Prelude library Data.List module.
*/
replicate(0, _, []).
replicate(Count, Elem, [Elem|Tail]) :-
    Count #>= 0,
    Count_1 #= Count - 1,
    replicate(Count_1, Elem, Tail).

/*
This is similar to Haskell's 'replicate', but bidirectional.

This works without clpfd.

This slightly differs from replicate/3.
It's magical how clpfd can shorten that program.
*/
repeat_thing_list(0, _, []) :- !.
repeat_thing_list(Repeat, Thing, [Thing | Tail]) :-
    integer(Repeat), !, Repeat > 0,
    Repeat1 is Repeat - 1,
    repeat_thing_list(Repeat1, Thing, Tail).
repeat_thing_list(N, A, B) :-
    list_count_thing_repeat(B, 0, A, N).

    list_count_thing_repeat([A | B], I, A, N) :- !, J is I + 1, list_count_thing_repeat(B, J, A, N).
    list_count_thing_repeat([], N, _, N) :- !.
