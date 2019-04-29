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

get_env_or_throw(Key, Val) :-
    getenv(Key, Val)
    ->  true
    ;   throw(error(environment_not_defined(Key))).

load_1 :-
    get_env_or_throw(my_prolog_home, Dir),
    absolute_file_name("boot/load1.pro", Abs, [relative_to(Dir)]),
    consult(Abs).

load_from_argv :-
    load_1,
    current_prolog_flag(argv, Argv),
    forall(member(Arg, Argv), (
        absolute_file_name(Arg, Abs), % relative to working directory
        my_consult(Abs)
    )).

do_profile :-
    profile(load_from_argv).

:- if(getenv('PROFILE','1')).
:- else.
:- initialization(load_from_argv, now).
:- endif.
