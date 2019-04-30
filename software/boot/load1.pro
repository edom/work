:- annotate(file,[
    purpose-"enable Prolog programming in the large"
    , tags-[
        architecture
    ]
]).

%   Usage: consult this file into a module,
%   and then call initialize/1 first before calling anything else.

:- export([
    my_consult/1
]).

:- section("our interface with the underlying Prolog interpreter").

    :- import(user,[
        assertz_into/2
        , goal_arg_meta/3
        , initialize_module/1
        , module_export/2
        , use_module_4/4
    ]).

    :- import(user,[
        debug/3
        , (dynamic)/1
        , (meta_predicate)/1
        , nb_current/2
        , nb_setval/2
        , print_message/2
        , prolog_load_context/2
        , stream_property/2
    ]).

:- end_section.

:- use_module(library(gensym),[
    gensym/2
]).
:- use_module(library(listing),[
    portray_clause/1
]).

:- include("functional.pro").
:- include("database.pro").
:- include("genmod.pro").
:- include("read.pro").
:- include("clause.pro").
:- include("check.pro").
:- include("import.pro").
:- include("link.pro").
:- include("troubleshoot.pro").

/*
This diagram describes how my_consult/1 works:

                             read_term/2
    file (in storage medium) ----------> file (parsed)

         inline include/1 directives
    file --------------------------> unit

         link clauses, qualify goals
    unit --------------------------> module

           assert linked clauses
    module --------------------> predicates you can call using my_call/1

*/

:- section("unit load context").

    context(A) :- functor(A,context,4).

    context_unit(A,B) :- context(A), arg(1,A,B).
    context_module(A,B) :- context(A), arg(2,A,B).
    context_term_count(A,B) :- context(A), arg(3,A,B).
    nb_set_context_term_count(A,B) :- nb_setarg(3,A,B).
    context_annotation(A,B) :- context(A), arg(4,A,B).
    nb_set_context_annotation(A,B) :- nb_setarg(4,A,B).

    context_new(File, Module, Context) :-
        context(Context),
        context_unit(Context, File),
        context_module(Context, Module),
        context_term_count(Context, 0),
        context_annotation(Context, []).

    origin(A) :- functor(A,origin,1).
    origin_file(A,B) :- origin(A), arg(1,A,B).

:- end_section.

:- section("consult").

:- annotate([
    purpose-"read, load, and link; like consult/1"
    , problem-"my_consult/1 should be able to fully handle its own source file (load1.pro) without $no_link hack"
]).
my_consult(Path) :- my_consult(Path, []).

:- annotate([
    todo(production, "implement conditional compilation")
]).
my_consult(Path, Opts) :-
    must_be(ground, Opts),
    absolute_file_name(Path, Unit, Opts),
    (   unit_module(Unit, Module)
    ->  true
    ;   generate_module_name(Unit, Module),
        initialize_module(Module),
        assertz(unit_module(Unit, Module))
    ),
    debug(load_unit_into_module, "my_consult: loading unit ~w into ~w", [Unit,Module]),
    context_new(Unit, Module, Context),
    read_file_abs(Context, Unit),
    (unit_linked(Unit)
    ->  true
    ;   assertz_once(unit(Unit)),
        (   unit_error(Unit, _)
        ->  print_unit_errors(Unit)
        ;   link_unit(Context),
            assertz(unit_linked(Unit))
        )
    ).

:- annotate([purpose="assert the linked clauses into the Prolog interpreter"]).
link_unit(Context) :-
    context_unit(Context, Unit),
    context_module(Context, Module),
    forall(unit_clause_linked(Unit, _, Linked, _),
        assertz_into(Module, Linked)
    ).

    unit_clause(Unit, Index, Clause, Origin) :-
        must_be(ground, Unit),
        findall(C-Origin, (
            unit_term(Unit, Index, Term, Origin),
            term_clause(Term, C)
        ), Clauses),
        nth1(Index, Clauses, Clause-Origin).

    unit_clause_linked(Unit, Index, Linked, Origin) :-
        unit_clause(Unit, Index, Clause, Origin),
        link_clause(Unit, Origin, Clause, Linked).

:- end_section.

% -------------------- directive

handle_use_module(Context, Rel, Imports) :-
    context_unit(Context, Unit),
    absolute_file_name(Rel, Abs, [
        relative_to(Unit),
        file_type(prolog),
        access(read)
    ]),
    (file_visited(Abs)
    ->  true
    ;   assertz(file_visited(Abs)),
        context_module(Context, Current),
        use_module_4(Current, Rel, Abs, Mod),
        assertz(unit_module(Abs,Mod)),
        assertz(unit_import_list(Unit,module(Mod),Imports))
    ).

handle_import(Unit, Source, Imports) :-
    (   is_list(Imports)
    ->  true
    ;   type_error(import_item_list, Imports)
    ),
    (   Source = file(Rel)
    ->  absolute_file_name(Rel, Abs, [relative_to(Unit)]),
        assertz(unit_import_list(Unit, unit(Abs), Imports)),
        my_consult(Abs)
    ;   member(Source, [system,user])
    ->  assertz(unit_import_list(Unit, Source, Imports))
    ;   type_error(import_source, Source)
    ).

% -------------------- throw

throw_error(Term) :-
    throw(error(Term,_)).

:- annotate([superseded_by = throw_file_read_error/2]).
throw_read_error(Context, Error) :-
    throw_error(read_error(Context,Error)).

throw_file_read_error(File, Error) :-
    throw_error(read_error(File,Error)).


% -------------------- DEBUG test

test :-
    my_consult("boot/load_demo.pro"),
    my_call(file("boot/load_demo.pro"):hello),
    my_call(file("boot/load_demo.pro"):goodbye).

initialize(MyAbsPath) :-
    load_this_file(MyAbsPath).

load_this_file(MyAbsPath) :-
    nb_current(my_loader_loaded, _)
    ->  true
    ;   nb_setval(my_loader_loaded, true),
        my_consult(MyAbsPath).
