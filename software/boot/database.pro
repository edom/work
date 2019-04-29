:- annotate(file,[
    purpose-"remember loaded things"
    , note-"internal state of the loader"
]).

:- section("relations populated by observing the external world").

    :- annotate(relation(unit_module/2),[cardinality=1:1]).

    %%  file_predicate(?File, ?Pred) is nondet.

    :- annotate(predicate(file_predicate/2),[
        meaning = "File defines NameArity"
    ]).

    :- dynamic file_visited/1. % File
    :- dynamic file_term/3. % File, Index, Term
    :- dynamic file_predicate/2. % File, NameArity
    :- dynamic file_include/2. % File, Include
    :- dynamic unit/1. % File
    :- dynamic unit_linked/1. % File
    :- dynamic unit_module/2. % File, Module
    :- dynamic unit_term/4. % File, Index, Term, Origin
    :- dynamic unit_import_list/3. % Importer, Exporter, Imports
    :- dynamic unit_export_list/2. % Exporter, Exports
    :- dynamic object_annotation/2. % Object, Annotation

:- end_section.

:- section("derived relations").

    unit_predicate(Unit, Pred) :-
        unit(Unit),
        file_predicate_trans(Unit, Pred).

        file_predicate_trans(File, Pred) :-
            file_predicate(File, Pred).

        file_predicate_trans(File, Pred) :-
            file_include(File, That),
            file_predicate(That, Pred).

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
