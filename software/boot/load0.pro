:- consult("debug.pro").

:- debug(load_unit_into_module). % DEBUG
%:- debug(import_expansion). % DEBUG

ignored_directive(annotate(_)).
ignored_directive(annotate(_,_)).
ignored_directive(export(_)).
ignored_directive(section(_)).
ignored_directive(end_section).

term_expansion(:- Dir, []) :- ignored_directive(Dir).

:- consult("load1.pro").

% -------------------- load files specified on command-line arguments

load_from_argv :-
    current_prolog_flag(argv, Argv),
    forall(
        member(Arg, Argv),
        my_consult(Arg)
    ).

:- initialization(load_from_argv).
