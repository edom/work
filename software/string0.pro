:- module(string0, [
    strings_join/2
    , strings_separator_join/3
]).

/** strings_join(Strings, String)

Strings is input.
Strings is a list of strings.

String is output.

Example:
strings_join(["abc", "def", "ghi"], "abcdefghi")

See also atomics_to_string/2.
*/
strings_join([], "") :- !.
strings_join([H|T], Output) :-
    strings_join(T, S),
    string_concat(H, S, Output).

/*
Consider the standard atomics_to_string/3 instead of this.

strings_separator_join(Strings, Separator, String)

This is unidirectional.
This can't be used to split strings.

Strings is input.
Strings is a list of strings.

Separator is a string.

String is output.
*/
strings_separator_join([], _, "") :- !.
strings_separator_join([H], _, H) :- !.
strings_separator_join([H|T], Sep, Str) :-
    strings_separator_join(T, Sep, ST),
    string_concat(H, Sep, SHS),
    string_concat(SHS, ST, Str).
