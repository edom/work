:- module(language_prolog_transput, [
    readstream_kb/2,
    readfile_kb/2,
    readstring_kb/2
]).
/** <module> Use Prolog to read Prolog knowledge base from a Prolog source file

Example:

==
main :-
    readfile_kb('./language_prolog_transput.pro',K),
    print_term(K,[]).
==
*/

/**
readstream_kb(+Stream,-List).
readfile_kb(+Path,-List).
readstring_kb(+String,-List).

Read the input as a knowledge base.
*/
readstream_kb(Stream,[Term|Tail]) :- readorfail(Stream,Term), !, readstream_kb(Stream,Tail).
readstream_kb(_,[]).

    readorfail(Stream,Term) :- read(Stream,Term), !, \+ (Term = end_of_file, at_end_of_stream(Stream)).

readfile_kb(Path,K) :-
    setup_call_cleanup(
        open(Path, read, Stream, [type(binary)]),
        readstream_kb(Stream, K),
        close(Stream)
    ).

readstring_kb(String,K) :-
    setup_call_cleanup(
        open_string(String, Stream),
        readstream_kb(Stream, K),
        close(Stream)
    ).
