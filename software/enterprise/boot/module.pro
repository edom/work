/** <module> Tailoring SWI-Prolog module system to our requirements

Usage:

    - Declare each module with module_definition/2.
    - Call load_modules/0.
    - Debug topics for library(debug):
        - `connector`: declarations, connections

Known problems:

    - A module should not be loaded using both this mechanism and use_module/2.
    Otherwise there will be a permission_error.
    - This confuses PlDoc.
    Maybe we should write our own documentation system.
    - SWI-Prolog 7.6.4 assumes that each file is loaded at most once.
    We may have to use the `stream` option of load_files/2 to go against this tendency.
    But then we lose reloading ability.

Things:

    - A _qcon_ (qualified connector name) is a Module:Name term.
    - A _pin_ is a Name/Arity term.
*/

% -------------------- debug

:- debug(module). % DEBUG
%:- debug(module_connection). % DEBUG

debug_module(Format, Args) :-
    string_concat("module: ", Format, Format0),
    debug(module, Format0, Args).

debug_module_connection(Format, Args) :-
    string_concat("module_connection: ", Format, Format0),
    debug(module_connection, Format0, Args).

:- include("imperative.pro").
:- include("internal/module_doc.pro").
:- include("internal/module_derive.pro").
:- include("internal/module_check.pro").
:- include("internal/module_deprecated.pro").
:- include("internal/module_swi.pro").
:- include("internal/module_load.pro").
