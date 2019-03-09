:- module(main,[
    main/1
    , run/0
]).
/** <module> Enterprise model/ontology?

*/

% ------- internals

:- use_module('./syntax.pro',[]).
:- use_module('./modules.pro',[]).
:- use_module('./ontology_systems.pro',[]).
:- use_module('./ontology_web_applications.pro',[]).
:- use_module('./ontology_relational_databases.pro',[]).
:- use_module('./ontology_java_programs.pro',[]).
:- use_module('./ontology_java_programs_write.pro',[]).
:- use_module('./translation_java.pro',[]).
%:- use_module('./sql.pro',[]).
%:- use_module('./translation.pro',[]).
%:- use_module('./java0.pro',[]).

% ------- configuration

translation_java:default_package_name('com.spacetimecat.java').
ontology_java_programs_write:output_dir("out").
ontology_java_programs_write:dry_run(true).

% ------- internal links

subsume(L,R) :-
    foreach(modules:module_host(L,Pred), subsume(L,R,Pred)).

subsume(L,R,Name/Arity) :-
    functor(Head,Name,Arity),
    (predicate_property(R:Head,defined)
    ->  assertz(L:Head :- R:Head)
    ;   print_message(warning, undefined_predicate(R:Name/Arity))
    ).

prolog:message(undefined_predicate(P)) -->
    ["Undefined predicate ~w"-[P]].

translation_java:recordtype_field(T,F,FN,FT) :-
    ontology_systems:recordtype_field(T,F),
    ontology_systems:field_name(F,FN),
    ontology_systems:field_type(F,FT).

:- subsume(translation_java, ontology_web_applications).
:- subsume(ontology_java_programs, translation_java).

% ------- load specs

load_spec(Module,File) :-
    print_message(informational,loading_module(Module,File)),
    Module:use_module('./syntax.pro'),
    foreach(modules:module_host(ontology_systems,Pred), Module:discontiguous(Pred)),
    foreach(modules:module_host(ontology_web_applications,Pred), Module:discontiguous(Pred)),
    foreach(modules:module_host(ontology_relational_databases,Pred), Module:discontiguous(Pred)),
    Module:consult(File),
    subsume(ontology_systems,Module),
    subsume(ontology_web_applications,Module),
    subsume(ontology_relational_databases,Module).

:- load_spec(spec_accounting,'spec/accounting.pro').
:- load_spec(spec_employee,'spec/employee.pro').

/** main(++Args)

Entry point.
*/
main([run]) :- !, run.
main(Args) :- throw(error(unknown_args(Args),_)).

prolog:error_message(unknown_args(Args)) -->
    ['unknown command-line arguments ~w'-[Args]].

/** run

This is called by main/1,
but can also be run directly from the interpreter prompt for testing.
*/
run :-
    ontology_java_programs:check_ontology,
    ontology_java_programs_write:generate.

:- if(current_prolog_flag(argv,[])).
:- else.
    :- initialization(main,main).
:- endif.
