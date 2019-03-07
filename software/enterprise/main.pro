:- module(main,[
    main/1
    , run/0
]).
:- use_module('./modules.pro',[load_modules/0]).
%:- use_module('./sql.pro',[]).
%:- use_module('./translation.pro',[]).
%:- use_module('./java0.pro',[]).

/** <module> Enterprise model

See the file spec.pro for usage.

Notes for the language designer:
If you change a module name, you may need to update these files:
    - main_module.pro
    - main_link_manual.pro
*/

% ------- module and linking -------

modules:module(A) :- module(A,_).
modules:module_file(Module,File) :- module(Module,Dic), member(file-File,Dic).
modules:module_import(Module,Import) :- module(Module,Dic), member(imports-Imports,Dic), member(Import,Imports).
modules:module_export(Module,Export) :- module(Module,Dic), member(exports-Exports,Dic), member(Export,Exports).
modules:module_guest(A,B) :- module_guest(A,B).

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
    load_modules,
    consult('./main_link_manual.pro').

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

/** main(++Args)

Entry point.
*/
main([run]) :- !, run.
main(Args) :- throw(error(unknown_args(Args),_)).

prolog:error_message(unknown_args(Args)) -->
    ['unknown command-line arguments ~w'-[Args]].

run :-
    ensure_linked,
    translation_java:generate.

:- if(current_prolog_flag(argv,[])).
:- else.
    :- initialization(main,main).
:- endif.
