:- annotate([
    purpose-"populate file_term/3 and file_term_expanded/3"
    , problem-"may leak streams; how portable is setup_call_cleanup/3?"
]).
read_file_abs(Context, File) :-
    (file_visited(File)
    ->  true
    ;   assertz(file_visited(File)),
        assert_default_imports(File),
        open(File, read, Stream, [eof_action(eof_code)]),
        read_stream(Context, File, Stream, 1, _),
        close(Stream)
    ).

    :- annotate([
        problem = "operators should be local"
    ]).
    read_stream(Context, File, Stream, Index, Index2) :-
        read_term_1(Stream, OptTerm),
        case(OptTerm,[
            some(Term) -> (
                assertz(file_term(File, Index, Term)),
                %   We have to do expansion at this stage because
                %   do_unit_include/3 annotation processing requires the next expanded term.
                my_expand_term(Term, Expanded),
                assertz(file_term_expanded(File, Index, Expanded)),
                handle_term(Context, File, Expanded),
                Index1 is Index+1,
                read_stream(Context, File, Stream, Index1, Index2)
            ),
            none -> (
                Index2 = Index
            )
        ]).

        read_term_1(Stream, OptTerm) :-
            read_term(Stream, Term, [
                singletons(warning)
                , syntax_errors(error)
            ]),
            (Term == end_of_file, stream_property(Stream, end_of_stream(past))
            ->  OptTerm = none
            ;   OptTerm = some(Term)).

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
    /*
    We should rethink the expansion of case/2 because such expansion may confuse the user.
    What if the user defines a predicate case/2?
    The user would be surprised to find it rewritten to something else.
    */
    % my_expand_goal(case(_,_), case(A,B), Z) :- !, Z = case(A,B).
    % my_expand_goal(_, case(A,B), Z) :- !, expand_case(case(A,B), Z).
    my_expand_goal(_, A, Z) :- !, A = Z.

:- end_section.

:- section("handle term").

    :- annotate([
        purpose = "collect annotations"
        , purpose = "expand includes"
        , purpose = "process directives"
        , purpose = "populate file_term/3"
        , purpose = "populate file_term_expanded/3"
        , problem = "should initialization/{1,2} be handled? should loading a file execute arbitrary code?"
        , problem = "discontiguous/1 directive not handled"
        , todo(production, "handle meta-predicates correctly")
        , todo(production, "make op/3 effects local to module")
        , todo(production, "limit recursion depth")
    ]).
    handle_term(Context, File, Term) :-
        context_unit(Context, Unit),
        context_module(Context, Module),
        %   TODO: Merge two phases into one, because
        %   op/3 directives require immediate interpretation the term we just read/expanded.
        %   We can't do it in two phase like this.
        %
        %   We should reuse SWI-prolog library/operators.pl.
        case(Term,[
            /*
                annotate(file,Ann) means annotate the file that contains the source code.
                That file is the file that contains the bytes that represent the textual source code.

                Many files can be loaded into the same module by include/1.
            */
            (:- annotate(file,Ann)) ->
                annotate_object(file(File), Ann),

            % TODO resolve N/A against imports
            (:- annotate(predicate(N/A),Ann)) ->
                annotate_object(file_predicate(File,N/A), Ann),

            % relation/1 is synonym for predicate/1 but hints finiteness and multidirectionality
            (:- annotate(relation(N/A),Ann)) ->
                annotate_object(file_predicate(File,N/A), Ann),

            (:- annotate(Ref,_)) ->
                throw_error(invalid_annotation_ref(Ref)),

            (:- annotate(Ann)) ->
                nb_set_context_annotation(Context, Ann),

            (:- discontiguous(Name/Arity)) -> (
                assertz_once(file_predicate(File, Name/Arity)),
                discontiguous(Module:Name/Arity)
            ),

            (:- dynamic(Name/Arity)) -> (
                assertz_once(file_predicate(File, Name/Arity)),
                dynamic(Module:Name/Arity)
            ),
            (:- dynamic(_)) ->
                throw_error(syntax_error(Term)),

            (:- multifile(Name/Arity)) -> (
                assertz_once(file_predicate(File, Name/Arity)),
                multifile(Module:Name/Arity)
            ),
            (:- multifile(_)) ->
                throw_error(syntax_error(File,Term)),

            (:- meta_predicate(Head)) ->
                meta_predicate(Module:Head),

            (:- include(Rel)) -> (
                absolute_file_name(Rel, Abs, [relative_to(File)]),
                assertz_once(file_include(File, Abs)),
                read_file_abs(Context, Abs)
            ),

            (:- export(Exports)) ->
                assertz(unit_export_list(Unit, Exports)),

            (:- import(Src,Imports)) ->
                handle_import(Unit, Src, Imports),

            (:- use_module(Rel,Imports)) ->
                handle_use_module(Context, Rel, Imports),

            (:- initialization(_)) -> (
                true
            ),
            (:- initialization(_,_)) -> (
                true
            ),

            (:- op(Precedence, Type, Name)) -> (
                op(Precedence, Type, Name)
            ),

            % section/1 tries to help people with non-folding text editors
            (:- section(_)) -> true,
            (:- end_section) -> true,

            (:- Dir) ->
                throw_read_error(Context, unknown_directive(Dir)),

            _ -> (
                context_term_count(Context, Count),
                UIndex is Count+1,
                nb_set_context_term_count(Context, UIndex),
                origin_file(Origin, File),
                (   term_clause(Term, Clause),
                    clause_head(Clause, Head),
                    goal_pred(Head, Pred)
                ->  assertz_once(file_predicate(File, Pred))
                ;   true
                ),
                assertz(unit_term(Unit, UIndex, Term, Origin)),
                term_object(File, Term, Object),
                context_annotation(Context, Ann),
                (Ann = []
                ->  true
                ;   annotate_object(Object, Ann),
                    nb_set_context_annotation(Context, [])
                )
            )
        ]).

    annotate_object(Obj, Ann) :-
        assertz(object_annotation(Obj,Ann)).

    term_object(File, Head:-_, Object) :- !,
        Object = file_predicate(File,Name/Arity),
        functor(Head,Name,Arity).

    term_object(File, Head, Object) :- !,
        Object = file_predicate(File,Name/Arity),
        functor(Head,Name,Arity).

:- end_section.
