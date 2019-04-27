:- annotate(file,[
    problem-"how do we disable SWI-Prolog module auto-loading?"
]).

:- section("implicit import by default for every unit").

    default_import(system,[
        (!)/0
        , (->)/2
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
        , assertz/1
        , atom/1
        , atomic_list_concat/2
        , call/1
        , call/2
        , call/3
        , call/4
        , close/1
        , current_predicate/1
        , fail/0
        , false/0
        , forall/2
        , format/2
        , functor/3
        , ground/1
        , integer/1
        , is/2
        , is_list/1
        , length/2
        , nb_setarg/3
        , nonvar/1
        , number/1
        , open/4
        , read/1
        , read/2
        , read_term/2
        , read_term/3
        , repeat/0
        , retractall/1
        , setof/3
        , string/1
        , string_codes/2
        , throw/1
        , true/0
        , var/1
        , write/1
        , writeln/1
    ]).

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
        , member/2
    ]).
    default_import(module(error),[
        domain_error/2
        , instantiation_error/1
        , type_error/2
    ]).

    :- section("SWI-Prolog-specific").

        default_import(system,[
            (@)/2
            , add_import_module/3
            , delete_import_module/2
            , module_property/2
        ]).
        default_import(user,[
            debug/3
        ]).

    :- end_section.

    :- section("our extensions").

        default_import(user,[   % our extensions
            '$my_phrase'/3
            , case/2            % should be replaced with goal expansion
        ]).

    :- end_section.

    assert_default_imports(Unit) :-
        forall(default_import(Src,Imports),
            assertz(unit_import_list(Unit,Src,Imports))
        ).

:- end_section.


:- section("import by derivation").

    unit_import(Importer,Exporter,Pred) :-
        unit_import_list(Importer,Exporter,List),
        member(Pred,List).

    unit_export(Exporter,Pred) :-
        unit_export_list(Exporter,List),
        member(Pred,List).

    object_import(unit(I),E,P) :-
        unit_import(I,E,P).

    object_export(system,Pred) :-
        current_predicate(system:Pred).

    object_export(user,Pred) :-
        current_predicate(user:Pred).

    object_export(module(M),Pred) :-
        module_property(M,exports(Es)),
        member(Pred,Es).

    object_export(unit(E),Pred) :-
        unit_export(E,Pred).

:- end_section.
