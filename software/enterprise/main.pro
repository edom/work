:- documentation:consult("library/java_program_doc.pro").

:- use_module("syntax.pro",[]).
:- use_module("language/pal_interpreter.pro",[]).

:- debug. % disable optimization, retain full stack trace
:- debug(connector).
:- debug(ontology).

%%  run is det.
%   This also be run directly from the interpreter prompt for testing.

run :-
    pipeline:check,
    pipeline:generate.

% -------------------- translation pipelines

:- pipeline:consult("../boot/module.pro").
:- pipeline:consult("main_module.pro").
:- pipeline:consult("main_schema.pro").

:- pipeline:load_modules.

main(Args) :-
    write(Args),
    nl.

:- initialization(main, main).
