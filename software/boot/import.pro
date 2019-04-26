:- section("import by default").

default_import(system,[
    (!)/0
    , (->)/2
    , (<)/2
    , (=)/2
    , (=<)/2
    , (==)/2
    , (>)/2
    , (>=)/2
    , (\+)/1
    , (\=)/2
    , (\==)/2
    , (=..)/2
    , absolute_file_name/2
    , absolute_file_name/3
    , atom/1
    , atomic_list_concat/2
    , call/1
    , call/2
    , call/3
    , call/4
    , fail/0
    , false/0
    , file_base_name/2
    , file_name_extension/3
    , format/2
    , ground/1
    , integer/1
    , is/2
    , is_list/1
    , string_codes/2
    , length/2
    , nonvar/1
    , number/1
    , read/1
    , read/2
    , read_term/2
    , read_term/3
    , string/1
    , throw/1
    , true/0
    , var/1
]).
default_import(user,[
    '$my_phrase'/3
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

assert_default_imports(File) :-
    forall(default_import(Src,Imports),
        assertz(unit_import_list(File,Src,Imports))
    ).

:- end_section.


:- section("import by derivation").

file_import(Importer,Exporter,Pred) :-
    unit_import_list(Importer,Exporter,List),
    member(Pred,List).

file_export(Exporter,Pred) :-
    unit_export_list(Exporter,List),
    member(Pred,List).

object_import(file(I),E,P) :-
    file_import(I,E,P).

object_export(system,Pred) :-
    current_predicate(system:Pred).

object_export(user,Pred) :-
    current_predicate(user:Pred).

object_export(module(M),Pred) :-
    module_property(M,exports(Es)),
    member(Pred,Es).

object_export(file(E),Pred) :-
    file_export(E,Pred).

:- end_section.
