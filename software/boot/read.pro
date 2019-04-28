:- annotate([
    purpose-"populate file_term/3"
    , problem-"may leak streams; how portable is setup_call_cleanup/3?"
]).
read_file_abs(Path, File, Opts) :-
    absolute_file_name(Path, File, Opts),
    (file_visited(File)
    ->  true
    ;   assertz(file_visited(File)),
        assert_default_imports(File),
        open(File, read, Stream, [eof_action(eof_code)]),
        read_stream(File, Stream, 1, _),
        close(Stream)
    ).

    read_stream(File, Stream, Index, Index2) :-
        read_term_1(Stream, OptTerm),
        case(OptTerm,[
            some(Term) -> (
                assertz(file_term(File,Index,Term)),
                Index1 is Index+1,
                read_stream(File, Stream, Index1, Index2)
            ),
            none -> (
                Index2 = Index
            )
        ]).

        read_term_1(Stream, OptTerm) :-
            read_term(Stream, Term, [
                singletons(warning)
                , syntax_errors(error)
            ]), !,
            (Term == end_of_file, stream_property(Stream, end_of_stream(past))
            ->  OptTerm = none
            ;   OptTerm = some(Term)).

        read_term_1(_, _) :-
            throw_error(syntax_error).
