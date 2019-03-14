:- module(prolog_customization,[
    load_module_from_file/2
    , do_module_import/2
]).

/** <module> Tailoring Prolog to our requirements

Dynamic module manipulation:
    - load_module_from_file/2
    - do_module_import/2

Dynamic module stuff may confuse make/0 and reloading.
If anything looks funny, restart the Prolog interpreter.
*/

:- reexport('./ontology.pro',[
    directive/1
    , declare_class/2
]).
:- reexport('./component.pro',[
    declare_plug/2
    , declare_socket/2
    , connect_plug_to_socket/2
    , connect_plug_to_socket/3
    , compile_aux_clauses_0/1
]).
:- reexport('./module.pro',[
    module_host/2
]).
:- use_module(library(ansi_term)). % Colorize outputs written from directives.
:- use_module(library(error)).



/** load_module_from_file(+Module,+File) is det.

load_files/2 with module(Module) and register(false) option.

The same file can be loaded into different modules, unlike use_module/2.
*/
load_module_from_file(Module, File) :-
    Module:load_files(File, [module(Module),register(false)]).



/** do_module_import(?Target, ?Import) is det.

A more convenient import/1.

Target is a module name.

Import is any of these:

    - a Source:Name/Arity term where Source is a module name
    - a list of such terms
*/

do_module_import(_, []) :- !.
do_module_import(Target, [A|Z]) :- !, do_module_import(Target, A), do_module_import(Target, Z).
do_module_import(Target, Source:Name/Arity) :- !, Target:import(Source:Name/Arity).
do_module_import(_, A) :- domain_error(importable, A).



% -------------------- workaround for lost exceptions thrown from directives

% This is for getting the backtrace of exceptions thrown from directives.

:- if(getenv('DEBUG',_)).

    :- debug.

    user:prolog_exception_hook(error(E,_), error(E,Backtrace), _, _) :-
        get_prolog_backtrace(32, Backtrace).

    prolog:message(error(E,C)) -->
        {print_prolog_backtrace(string(S), C)},
        ["~w~n~w~n"-[E,S]].

:- endif.
