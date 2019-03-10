:- module(main_library,[
    load_spec/2
    , subsume/2
    , subsume/3
]).
/** <module> Common things encountered when writing a main module

Functionalities that help drive the translation process.
*/
:- use_module('./modules.pro',[]).

subsume(L,R) :-
    foreach(modules:module_host(L,Pred), subsume(L,R,Pred)).

subsume(L,R,Name/Arity) :-
    functor(Head,Name,Arity),
    (predicate_property(R:Head,defined)
    ->  assertz(L:Head :- R:Head)
    ;   print_message(warning, subsume_undefined_predicate(L,R,Name/Arity))
    ).

prolog:message(subsume_undefined_predicate(L,R,P)) -->
    ["Subsumption ~w :- ~w failed for predicate ~w"-[L,R,P]].

load_spec(Module,File) :-
    print_message(informational,loading_module(Module,File)),
    Module:use_module('./syntax.pro'),
    %foreach(modules:module_host(ontology_system,Pred), Module:discontiguous(Pred)),
    %foreach(modules:module_host(ontology_web_application,Pred), Module:discontiguous(Pred)),
    %foreach(modules:module_host(ontology_relational_databases,Pred), Module:discontiguous(Pred)),
    Module:consult(File).
