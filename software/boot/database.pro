:- annotate(file,[
    purpose-"remember loaded things"
    , note-"internal state of the loader"
]).

:- section("relations populated by observing the external world").

    :- annotate(relation(unit_module/2),[cardinality=1:1]).

    :- dynamic file_visited/1. % File
    :- dynamic file_term/3. % File, Index, Term
    :- dynamic unit/1. % File
    :- dynamic unit_linked/1. % File
    :- dynamic unit_module/2. % File, Module
    :- dynamic unit_term/4. % File, Index, Term, Origin
    :- dynamic unit_import_list/3. % Importer, Exporter, Imports
    :- dynamic unit_export_list/2. % Exporter, Exports
    :- dynamic object_annotation/2. % Object, Annotation

:- end_section.

:- section("derived relations").

    %%  file_predicate(?File,-Pred) is nondet.

    :- annotate([
        meaning = "File defines Pred"
    ]).

    % TODO: Speed this up: Turn this into a dynamic predicate.
    file_predicate(File, Pred) :-
        (var(File) -> true ; F = File),
        setof(F-P, file_pred(F,P,[]), Ps),
        member(File-Pred, Ps).

        file_pred(File, Pred, Opts) :-
            must_be(ground, Opts),
            file_term(File, _, Term),
            case(Term,[
                (:- dynamic(N/A)) -> (
                    Pred = N/A
                ),
                (:- include(That)) -> (
                    member(follow_includes(true), Opts)
                    ->  absolute_file_name(That, Abs, [relative_to(File)]),
                        file_pred(Abs, Pred, Opts)
                ),
                _ -> (
                    term_clause(Term, Clause),
                    clause_head(Clause, Head),
                    goal_pred(Head, Pred)
                )
            ]).

    unit_predicate(Unit, Pred) :-
        unit(Unit),
        (var(Pred) ->
            setof(P, file_pred(Unit,P,[follow_includes(true)]), Ps),
            member(Pred, Ps)
        ;   once(file_pred(Unit, Pred, [follow_includes(true)]))
        ).

    get_unit_module_or(Unit, Module, Alt) :-
        unit_module(Unit, Module)
        ->  true
        ;   case(Alt,[
                generate -> (
                    generate_module_name(Unit, Module),
                    assertz(unit_module(Unit,Module)),
                    initialize_module(Module)
                ),

                throw ->
                    throw_error(unit_module_failed(Unit)),

                fail ->
                    fail,

                _ ->
                    throw_error(invalid_alternative(Alt))
            ]).

:- end_section.

:- section("dynamic predicates").

    assertz_once(G) :-
        must_be(ground, G),
        call(G) -> true ; assertz(G).

:- end_section.
