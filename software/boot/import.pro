:- annotate(file,[
    problem-"how do we disable SWI-Prolog module auto-loading?"
]).

:- section("implicit import by default for every unit").

    default_import(system,[
        (!)/0
        , (,)/2
        , (->)/2
        , (;)/2
        , (<)/2
        , (=)/2
        , (=..)/2
        , (=<)/2
        , (==)/2
        , (>)/2
        , (>=)/2
        , (\+)/1
        , (\=)/2
        , (\==)/2
        , arg/3
        , atom/1
        , atom_string/2
        , atomic_list_concat/2
        , between/3
        , call/1
        , call/2
        , call/3
        , call/4
        , call_cleanup/2
        , close/1
        , current_op/3
        , current_predicate/1
        , fail/0
        , false/0
        , forall/2
        , foreach/2
        , functor/3
        , get_time/1
        , ground/1
        , integer/1
        , is/2
        , is_list/1
        , length/2
        , nb_setarg/3
        , nl/0
        , nonvar/1
        , number/1
        , number_string/2
        , once/1
        , op/3
        , repeat/0
        , setof/3
        , string/1
        , string_codes/2
        , throw/1
        , true/0
        , var/1
    ]).

    :- section("dynamic database").

        default_import(system,[
            (discontiguous)/1
            , (dynamic)/1
            , (multifile)/1
            , asserta/1
            , assertz/1
            , asserta/2
            , assertz/2
            , clause/2
            , erase/1
            , retract/1
            , retractall/1
        ]).

    :- end_section.

    :- section("non-standard string").

        default_import(system,[
            string_concat/3
        ]).

    :- end_section.

    :- section("transput").

        default_import(system,[
            current_output/1
            , format/2
            , format/3
            , open/4
            , read/1
            , read/2
            , read_term/2
            , write/1
            , writeln/1
        ]).

    :- end_section.

    :- section("non-standard transput").

        default_import(system,[
            copy_stream_data/2
            , read_term/3
        ]).

    :- end_section.

    :- section("file system").

        default_import(system,[
            absolute_file_name/2
            , absolute_file_name/3
            , file_base_name/2
            , file_name_extension/3
        ]).

    :- end_section.

    default_import(user,[
        must_be/2
    ]).

    default_import(module(lists),[
        append/3
        , findall/3
        , member/2
        , nth0/3
        , nth1/3
    ]).
    default_import(module(error),[
        domain_error/2
        , instantiation_error/1
        , type_error/2
    ]).

    :- section("our extensions").

        default_import(user,[   % our extensions
            '$my_phrase'/3
            , case/2            % should be replaced with goal expansion
            , phrase/2          % should calls to phrase(A,B) be linked to '$my_phrase'(A,B,[]) instead?
        ]).

    :- end_section.

    assert_default_imports(Unit) :-
        forall(default_import(Src,Imports),
            assertz(unit_import_list(Unit,Src,Imports))
        ).

:- end_section.


:- section("import by derivation").

    unit_export(Exporter,Pred) :-
        unit_export_list(Exporter,List),
        member(Pred,List).

    object_export(system,Pred) :-
        current_predicate(system:Pred).

    object_export(user,Pred) :-
        current_predicate(user:Pred).

    object_export(module(M),Pred) :-
        module_export(M, Pred).

    object_export(unit(E),Pred) :-
        unit_export(E,Pred).

:- end_section.
