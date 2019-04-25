read_term_1(Stream, OptTerm) :-
    read_term(Stream, Term, [syntax_errors(error)]), !,
    (Term == end_of_file, stream_property(Stream, end_of_stream(past))
    ->  OptTerm = none
    ;   OptTerm = some(Term)).

read_term_1(_, _) :-
    throw_error(syntax_error).

read_stream(Stream, List) :-
    read_term_1(Stream, OptTerm),
    case(OptTerm,[
        some(Term) -> (
            List = [Term|Rest],
            read_stream(Stream, Rest)
        ),
        none ->
            List = []
    ]).
