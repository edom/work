:- consult("debug.pro").
:- include("imperative.pro").
:- include("genmod.pro").

:- debug(load). % DEBUG
%:- debug(import_expansion). % DEBUG

% -------------------- process import/2 directives

system:term_expansion(:- import(Src,Imps), Exps) :-
    load_source_once(Src, Mod),
    findall(Exp, (member(Imp,Imps), import_expansion(Mod,Imp,Exp)), Exps).

% -------------------- actual loading of sources

% Keep track of loaded files.
:- dynamic module_file/2.

load_source_once(file(Rel), Mod) :- !,
    prolog_load_context(file, Src),
    absolute_file_name(Rel, Abs, [relative_to(Src)]),
    (module_file(Mod, Abs)
    ->  true
    ;   generate_module_name(Abs, Mod),
        debug(load, "abs_load_once: loading file ~w into ~w", [Abs,Mod]),
        '@'(system:load_files(Abs,[module(Mod)]), Mod),
        assertz(module_file(Mod,Abs))
    ).

load_source_once(A, _) :- !, type_error(import_source, A).

% -------------------- import_expansion/2

import_expansion(Mod, Name/Arity, Exp) :- !,
    check_predicate(defined(Mod:Name/Arity)),
    Exp = (Head :- Mod:Head),
    functor(Head, Name, Arity),
    debug(import_expansion, "import_expansion: ~w", [Exp]).

import_expansion(Mod, Name//DcgArity, Exp) :- !,
    Arity is DcgArity + 2,
    import_expansion(Mod, Name/Arity, Exp).

import_expansion(Mod, Name/Arity as Alias, Exp) :- !,
    check_predicate(defined(Mod:Name/Arity)),
    Exp = (Renamed :- Mod:Orig),
    functor(Orig, Name, Arity),
    functor(Renamed, Alias, Arity),
    foreach(between(1,Arity,N), arg_unify(N,Orig,Renamed)),
    debug(import_expansion, "import_expansion: ~w", [Exp]).

import_expansion(Mod, multifile(Name/Arity), Exp) :- !,
    check_predicate(multifile(Mod:Name/Arity)),
    Exp = (Mod:Head :- Head),
    functor(Head, Name, Arity),
    debug(import_expansion, "import_expansion: ~w", [Exp]).

import_expansion(Mod, multifiles(List), Exps) :- !,
    findall(
        Exp,
        (member(Spec,List), import_expansion(Mod,multifile(Spec),Exp)),
        Exps
    ).

import_expansion(_, A, _) :- !,
    type_error(import_spec, A).

check_predicate(defined(Module:Name/Arity)) :- !,
    functor(Head, Name, Arity),
    (predicate_property(Module:Head, defined)
    ->  true
    ;   existence_error(predicate, Module:Name/Arity)).

check_predicate(multifile(Module:Name/Arity)) :- !,
    check_predicate(defined(Module:Name/Arity)),
    functor(Head, Name, Arity),
    (predicate_property(Module:Head, multifile)
    ->  true
    ;   type_error(multifile_predicate, Module:Name/Arity)).

check_predicate(Exp) :- !,
    type_error(check_predicate_exp, Exp).

%%  arg_unify(?N, ?Functor1, ?Functor2) is nondet.
%   Unify argument N of Functor1 and argument N of Functor2.

arg_unify(N,F,G) :- arg(N,F,A), arg(N,G,A).

% -------------------- load files specified on command-line arguments

:-  current_prolog_flag(argv, Argv),
    forall(
        member(Arg, Argv),
        consult(Arg)
    ).
