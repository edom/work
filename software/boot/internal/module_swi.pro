% -------------------- codes specific to SWI-Prolog 7.6.4

/** consult_unregistered(+File) is det.
    consult_unregistered_into_module(+File, +Module) is det.

This is a variant of consult/1 that can load the same file many times to many modules.
See also load_files/2.

The connotation of consult/1 is that the file should not be a module file.
That is, the file should not begin with a module/2 directive.

consult_unregistered/1 loads into the context module.

This uses load_files/2 with module(Module) and register(false) option.

The same file can be loaded into several different modules, unlike use_module/2.

Example:

==
% -------------------- main.pro

:- consult_unregistered(a, "a.pro").
:- consult_unregistered(b, "a.pro").

a:p(0).
b:p(1).

% -------------------- a.pro

:- multifile p/1.

% -------------------- query

?- a:p(A).
A = 0.

?- b:p(A).
A = 1.
==

Known problems:

    - This may confuse edit/1, listing/1, PlDoc, the IDE, and reloading.
    If anything looks funny, restart the Prolog interpreter.

If we want a file to be _shared_, we use use_module/2.
If we don't want a file to be shared, we use consult_unregistered/2.

See also:

    - `colon_sets_calling_context` flag in current_prolog_flag/2
    - 2011 article "Meta-predicate Semantics" by Paulo Moura in page 160 of LNCS 7225, LOPSTR 2011
    - XSB Prolog module system (atom-based vs predicate-based) http://xsb.sourceforge.net/shadow_site/manual1/node18.html

Developer notes:

    - If File is not a module file, then load_files(File,Opts) tries to import all predicates in File into the _context module_,
    unless load_files/2 is called with the option imports([]).

    - There are two ways to set the context module in SWI-Prolog 7.6.4:
        - by module qualification: `M:pred`
        - by the at-sign operator '@'/2: '@'(pred,M)
*/

:- meta_predicate consult_unregistered(:).

consult_unregistered(File) :-
    load_files(File, [register(false)]).

consult_unregistered_into_module(File, Module) :-
    '@'(
        system:load_files(File, [
            module(Module)
            , register(false)
        ]),
        Module
    ).
