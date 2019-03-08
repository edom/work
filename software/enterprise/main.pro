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
:- use_module('./translation_java.pro',[]).
%:- use_module('./sql.pro',[]).
%:- use_module('./translation.pro',[]).
%:- use_module('./java0.pro',[]).

load_spec(Module,File) :-
    print_message(informational,loading_module(Module,File)),
    Module:use_module('./syntax.pro'),
    foreach(modules:module_host(ontology_systems,Pred), Module:discontiguous(Pred)),
    foreach(modules:module_host(ontology_web_applications,Pred), Module:discontiguous(Pred)),
    foreach(modules:module_host(ontology_relational_databases,Pred), Module:discontiguous(Pred)),
    Module:consult(File),
    foreach(modules:module_host(ontology_systems,Pred), link(ontology_systems,Module,Pred)).

    link(L,R,Name/Arity) :-
        functor(Head,Name,Arity),
        assertz(L:Head :- R:Head).

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
    translation_java:generate.

:- if(current_prolog_flag(argv,[])).
:- else.
    :- initialization(main,main).
:- endif.



% Everything below this is deprecated and is no longer used.



% ------- module and linking -------

modules:module(A) :- module(A,_).
modules:module_file(Module,File) :- module(Module,Dic), member(file-File,Dic).
modules:module_import(Module,Import) :- module(Module,Dic), member(imports-Imports,Dic), member(Import,Imports).
modules:module_export(Module,Export) :- module(Module,Dic), member(exports-Exports,Dic), member(Export,Exports).
modules:module_guest(A,B) :- module_guest(A,B).
modules:module_instantiates_ontology(A,B) :- module_instantiates_ontology(A,B).
modules:ontology(A) :- ontology(A,_).
modules:ontology_file(A,B) :- ontology(A,L), member(file-B,L).
modules:ontology_module(A,B) :- ontology(A,L), member(module-B,L).

:- include('./main_module.pro').

:- dynamic linked/0.

ensure_linked :- \+linked, !, assertz(linked), link.
ensure_linked.

/** link

Load the Prolog knowledge base.

SWI-Prolog 7.6.4 pldoc fails with "Undefined procedure: accounting:'$pldoc_link'/2":
if load_modules is called with module prefix.
To reproduce that error, start =|swipl -s main.pro|= and query this:

==
doc_server(4001).
modules:load_modules.
==

This also reproduces the error if =|swipl|= is started without any arguments.

==
doc_server(4001).
consult(main).
==

Another way to reproduce the error:
Start =|swipl --pldoc=4001 -s main.pro|=
and open =|http://localhost:4001/pldoc/doc/_CWD_/index.html|=.
*/
link :-
    find_specs,
    modules:load_ontologies,
    modules:load_modules.

    find_specs :-
        \+ file_search_path(spec,_), !,
        throw(error(no_spec_search_path,_)).

    find_specs :-
        findall(SpecFile, specfile(SpecFile), SpecFiles),
        print_message(informational,spec_files(SpecFiles)).

    prolog:error_message(no_spec_search_path) -->
        ['spec search path not set\n'],
        ['Hint: If you are using swipl, try adding -p spec=PATH command-line argument,\n'],
        ['where PATH refers to a directory that contains your Prolog specification files.\n'].

    prolog:message(spec_files(Files)) -->
        {length(Files,N)},
        ['Found ~w spec files: ~w'-[N,Files]].
