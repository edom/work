:- module(prolog_customization,[
    load_module_from_file/2
    , do_module_import/2
    , throw_error/1
]).

/** <module> Tailoring Prolog to our requirements

Dynamic module manipulation:
    - load_module_from_file/2
    - do_module_import/2

Dynamic module stuff may confuse make/0 and reloading.
If anything looks funny, restart the Prolog interpreter.
*/

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

throw_error(E) :- throw(error(E,_)).
