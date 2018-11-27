/*
Should this be named array or vector?

This assumes the Prolog implementation doesn't limit functor arity.
*/
:- module(array, [
    % length
    array_length/2
    % list
    , array_list/2
    % access
    , array_index/2
    , array_index_value/3
    , array_value/3
    % comparison
    , array_equal_at/3
    , array_length_equal/3
    , array_copy_except/3
]).

:- use_module(library(clpfd)).

/**
Can be used to create a new array with bound length but unbound elements.

At least one parameter must be bound.
*/
array_length(Array, Length) :- compound_name_arity(Array, array, Length), !.

/*
Can be used to convert between array and list.
*/
array_list(Array, List) :- var(Array), var(List), !, throw('array_list: arguments not sufficiently instantiated').
array_list(Array, List) :- var(Array), !, array_from_list_(Array, List).
array_list(Array, List) :- var(List), !, array_to_list_(Array, List).
array_list(Array, List) :- array_length(Array, N), length(List, N), array_compare_list_(Array, 0, N, List).

    array_compare_list_(Array, Begin, End, [H | T]) :-
        array_index_value(Array, Begin, H),
        !,
        Begin1 #= Begin + 1,
        array_compare_list_(Array, Begin1, End, T).
    array_compare_list_(_, Begin, End, []) :- Begin = End.

    array_from_list_(A, L) :- length(L, N), array_length(A, N), array_compare_list_(A, 0, N, L).

    array_to_list_(A, L) :- array_length(A, N), array_compare_list_(A, 0, N, L).

/*
See array_index_value/3.

Can be used to enumerate the indexes of an array.
*/
array_index(Array, Index) :- array_index_value(Array, Index, _).

/*
See array_index_value/3.
*/
array_value(Array, Value) :- array_index_value(Array, _, Value).

/*
At least one parameter must be bound.

Can be used to enumerate an array.

Can be used to find the occurrences of a value in an array.

Can be used to get an element of an array.

Can be used to set an element of an array if that element is unset.
*/
array_index_value(Array, Index, Value) :- N #= Index + 1, arg(N, Array, Value).

array_equal_at(A, B, I) :- nonvar(A), !, array_index_value(A, I, V), array_index_value(B, I, V).
array_equal_at(A, B, I) :- nonvar(B), !, array_index_value(B, I, V), array_index_value(A, I, V).
array_equal_at(_, _, _) :- instantiation_error(array_equal_at/3).

array_copy_except(A, B, K) :-
    array_length_equal(A, B, N),
    compare_(A, B, 0, N, K).

    compare_(_, _, N, N, _) :- !.
    compare_(A, B, K, N, K) :- !, K1 #= K + 1, compare_(A, B, K1, N, K).
    compare_(A, B, I, N, K) :- array_equal_at(A, B, I), I1 #= I + 1, compare_(A, B, I1, N, K).

array_length_equal(A, B, N) :- nonvar(N), !, array_length(A, N), array_length(B, N).
array_length_equal(A, B, N) :- nonvar(A), !, array_length(A, N), array_length(B, N).
array_length_equal(A, B, N) :- nonvar(B), !, array_length(B, N), array_length(A, N).
array_length_equal(_, _, _) :- instantiation_error(array_length_equal/3).
