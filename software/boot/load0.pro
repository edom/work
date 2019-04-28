:- consult("debug.pro").

:- debug(load_unit_into_module). % DEBUG
%:- debug(import_expansion). % DEBUG

ignored_directive(annotate(_)).
ignored_directive(annotate(_,_)).
ignored_directive(export(_)).
ignored_directive(import(_,_)).
ignored_directive(section(_)).
ignored_directive(end_section).

term_expansion(:- Dir, []) :- ignored_directive(Dir).

% -------------------- SWI-Prolog-specific predicates

    initialize_module(Module) :-
        Module:set_module(base(system)),
        Module:set_module(class(system)),
        delete_import_module(Module, user),
        add_import_module(Module, system, end).

    assertz_into(Module, Clause) :-
        '@'(system:assertz(Clause), Module).

    use_module_4(Importer, ExpRel, ExpAbs, ExpMod) :-
        '@'(system:use_module(ExpAbs), Importer),
        (source_file_property(ExpAbs, module(ExpMod))
        ->  true
        ;   throw(error(impossible(use_module(ExpRel),ExpAbs),_))).

    module_export(M, Pred) :-
        module_property(M,exports(Es)),
        member(Pred,Es).

    goal_arg_meta(Goal, Arg, M) :-
        predicate_property(Goal, meta_predicate(Meta)), !,
        arg(Arg, Meta, M).

    goal_arg_meta(_, _, ?).


% -------------------- load files specified on command-line arguments

load_from_argv :-
    consult("load1.pro"),
    current_prolog_flag(argv, Argv),
    forall(member(Arg, Argv), (
        absolute_file_name(Arg, Abs),
        my_consult(Abs)
    )).

% for profiling

profile_main :-
    consult("boot/load1.pro"),
    current_prolog_flag(argv, Argv),
    forall(
        member(Arg, Argv),
        % my_call(unit("boot/load1.pro"):my_consult(Arg))
        my_consult(Arg)
    ).

:- if(getenv('PROFILE','1')).
:- else.
:- initialization(load_from_argv, now).
:- endif.
