:- annotate([
    purpose-"populate file_term/3 and file_term_expanded/3"
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
                assertz(file_term(File, Index, Term)),
                %   We have to do expansion at this stage because
                %   do_unit_include/3 annotation processing requires the next expanded term.
                my_expand_term(Term, Expanded),
                assertz(file_term_expanded(File, Index, Expanded)),
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

:- section("term expansion and goal expansion").

    my_expand_term(A, Z) :- var(A), !, A = Z.

    my_expand_term((Head :- Body), Z) :- !,
        Z = (Head :- Body1),
        my_expand_goal(Head, Body, Body1).

    my_expand_term(A, Z) :- !, A = Z.

    my_expand_goal(H, (A,B), (Y,Z)) :- !, my_expand_goal(H,A,Y), my_expand_goal(H,B,Z).
    my_expand_goal(H, (A;B), (Y;Z)) :- !, my_expand_goal(H,A,Y), my_expand_goal(H,B,Z).
    my_expand_goal(H, (A->B), (Y->Z)) :- !, my_expand_goal(H,A,Y), my_expand_goal(H,B,Z).
    my_expand_goal(H, (\+A), (\+Z)) :- !, my_expand_goal(H,A,Z).
    my_expand_goal(case(_,_), case(A,B), Z) :- !, Z = case(A,B).
    my_expand_goal(_, case(A,B), Z) :- !, expand_case(case(A,B), Z).
    my_expand_goal(_, A, Z) :- !, A = Z.

:- end_section.
