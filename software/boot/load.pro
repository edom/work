:-  include("module.pro").
:-  consult("debug.pro").
:-  documentation:consult("doc.pro").

:- debug(load). % DEBUG
%:- debug(import_expansion). % DEBUG

% -------------------- process import/2 directives

system:term_expansion(:- import(Src,Imps), Exps) :-
    load_source_once(Src, Mod),
    findall(Exp, (member(Imp,Imps), import_expansion(Mod,Imp,Exp)), Exps).

:- dynamic module_file/2.

load_source_once(file(Rel), Mod) :- !,
    prolog_load_context(file, Src),
    absolute_file_name(Rel, Abs, [relative_to(Src)]),
    abs_load_once(Abs, Mod).

load_source_once(A, _) :- !, type_error(import_source, A).

abs_load_once(Abs, Mod) :- module_file(Mod, Abs), !.
abs_load_once(Abs, Mod) :- !,
    gensym('genmod', Gen),
    file_base_name(Abs, FileName),
    file_name_extension(Base, _, FileName),
    atomic_list_concat([Gen,'_',Base], Mod),
    debug(load, "abs_load_once: loading file ~w into ~w", [Abs,Mod]),
    '@'(system:load_files(Abs,[module(Mod)]), Mod),
    assertz(module_file(Mod,Abs)).

import_expansion(Mod, Name/Arity, Exp) :- !,
    check_predicate(defined(Mod:Name/Arity)),
    Exp = (Head :- Mod:Head),
    functor(Head, Name, Arity),
    debug(import_expansion, "import_expansion: ~w", [Exp]).

import_expansion(Mod, ForeignName/Arity as Alias, Exp) :- !,
    check_predicate(defined(Mod:ForeignName/Arity)),
    Exp = (Renamed :- Mod:Orig),
    functor(Orig, ForeignName, Arity),
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

import_expansion(_, A, _) :- !, type_error(import_spec, A).

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
