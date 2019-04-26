:- annotate(file,[
    purpose-"enable Prolog programming in the large"
    , tags-[
        architecture
    ]
]).

:- use_module(library(gensym),[
    gensym/2
]).

:- include("functional.pro").
:- include("genmod.pro").
:- include("read.pro").
:- include("dcg.pro").
:- include("swi.pro").
:- include("check.pro").
:- include("import.pro").
:- include("link.pro").
:- include("user.pro").

:- export([
    my_consult/1
]).


:- section("database").

:- dynamic file_visited/1. % File
:- dynamic file_stream/2. % File, Stream
:- dynamic file_predicate/2. % File, Name/Arity
:- dynamic file_term/3. % File, Index, Term
:- dynamic unit_module/2. % File, Module
:- dynamic unit_term/4. % File, Index, Term, Origin
:- dynamic unit_import_list/3. % Importer, Exporter, Imports
:- dynamic unit_export_list/2. % Exporter, Exports
:- dynamic module_clause/3. % Module, Clause, Origin
:- dynamic object_annotation/2. % Object, Annotation

:- end_section.


:- section("unit load context").

context(A) :- functor(A,context,3).

context_unit(A,B) :- context(A), arg(1,A,B).
context_module(A,B) :- context(A), arg(2,A,B).
context_term_count(A,B) :- context(A), arg(3,A,B).
nb_set_context_term_count(A,B) :- nb_setarg(3,A,B).

context_new(File, Module, Context) :-
    context(Context),
    context_unit(Context, File),
    context_module(Context, Module),
    context_term_count(Context, 0).

:- end_section.


% -------------------- consult

:- annotate([
    purpose-"read, load, and link; like consult/1"
    , problem-"my_consult/1 should be able to fully handle its own source file (load1.pro) without $no_link hack"
]).
my_consult(Path) :- my_consult(Path, []).

my_consult(Path, Opts) :-
    must_be(ground, Opts),
    read_file_abs(Path, File, []),
    File = Unit,
    (unit_module(Unit, _)
    ->  true
    ;   generate_module_name(Unit, Module),
        debug(load_unit_into_module, "my_consult: loading unit ~w into ~w", [Unit,Module]),
        assertz(unit_module(Unit,Module)),
        initialize_module(Module),
        context_new(Unit, Module, Context),
        do_unit_include(Context, Unit),
        interpret_directives(Context),
        (member('$no_link', Opts)
        ->  true
        ;   my_check,
            (error(_)
            ->  true
            ;   actuate_unit(Context)
            )
        )
    ).


    :- annotate([purpose-"populate unit_term/4"]).
    do_unit_include(Context, File) :-
        forall(file_term(File, FIndex, Term), (
            case(Term,[
                (:- include(Rel)) -> (
                    read_file_abs(Rel, Abs, [relative_to(File)]),
                    do_unit_include(Context, Abs)
                ),
                _ -> (
                    context_unit(Context, Unit),
                    context_term_count(Context, Count),
                    UIndex is Count+1,
                    nb_set_context_term_count(Context, UIndex),
                    resolve0(Term, File, Term1),
                    assertz(
                        unit_term(Unit, UIndex, Term1, file_term(File,FIndex,Term))
                    )
                )
            ])
        )).

        resolve0(:- annotate(file,Ann), File, Z) :- !,
            Z = (:- annotate(file(File),Ann)).

        resolve0(A, _, A).

:- annotate([
    purpose-"populate file_term/3"
    , problem-"may leak streams; how portable is setup_call_cleanup/3?"
]).
read_file_abs(Path, File, Opts) :-
    absolute_file_name(Path, File, Opts),
    (file_visited(File)
    ->  true
    ;   assertz(file_visited(File)),
        assert_default_imports(File),
        open(File, read, Stream, [eof_action(eof_code)]),
        assertz(file_stream(File,Stream)),
        read_stream(File, 1, _),
        retractall(file_stream(File,Stream)),
        close(Stream)
    ).

    read_stream(File, Index, Index2) :-
        file_stream(File, Stream),
        read_term_1(Stream, OptTerm),
        case(OptTerm,[
            some(Term) -> (
                assertz(file_term(File,Index,Term)),
                Index1 is Index+1,
                read_stream(File, Index1, Index2)
            ),
            none -> (
                Index2 = Index
            )
        ]).

actuate_unit(Context) :-
    context_unit(Context, Unit),
    context_module(Context, Module),
    forall(unit_clause(Unit, Index, Clause, Origin), (
        link_unit_clause(Unit, Clause, Linked),
        assertz(module_clause(Module, Linked, unit_clause(Unit, Index, Clause, Origin))),
        assert_2(Module, Linked)
    )).

    unit_clause(Unit, Index, Clause, Origin) :-
        unit_term(Unit, Index, Term, Origin),
        term_clause(Term, Clause).

        :- annotate([purpose-"translate a read term to a clause"]).
        term_clause(:- _, _) :- !, fail.
        term_clause(A :- B, Z) :- !, Z = (A :- B).
        term_clause(A --> B, Z) :- !, dcg_clause(A --> B, Z).
        term_clause(A, Z) :- !, Z = A.

% -------------------- load

:- annotate([
    problem-"dynamic/1 not handled"
    , problem-"initialization/2 not handled"
]).
interpret_directives(Context) :-
    context_unit(Context, Unit),
    repeat, (
        unit_term(Unit, Index, Term, _),
        case(Term,[
            (:- Dir) -> case(Dir,[
                annotate(Ann) -> (
                    Index1 is Index+1,
                    unit_term(Unit, Index1, Term1, _),
                    term_object(Context, Term1, Object),
                    annotate_object(Object, Ann)
                ),
                /*
                    annotate(file,Ann) means annotate the file that contains the source code.
                    That file is the file that contains the bytes that represent the textual source code.

                    Many files can be loaded into the same module by include/1.
                */
                annotate(Ref,Ann) -> (
                    resolve_annref(Context, Ref, Obj),
                    annotate_object(Obj, Ann)
                ),
                dynamic(_) -> (
                    true
                ),
                export(Exports) ->
                    assertz(unit_export_list(Unit,Exports)),
                import(Src,Imports) ->
                    handle_import(Unit, Src, Imports),
                include(Rel) -> (
                    read_file_abs(Rel, _, [relative_to(Unit)])
                ),
                initialization(_,_) -> (
                    true
                ),
                use_module(Rel,Imports) ->
                    handle_use_module(Context,Rel,Imports),
                % section/1 tries to help people with non-folding text editors
                section(_) -> true,
                end_section -> true,
                _ ->
                    throw_read_error(Context, unknown_directive(Dir))
            ]),
            _ -> (
                term_clause(Term, Clause),
                clause_head(Clause, Head),
                functor(Head, Name, Arity),
                assertz_once(file_predicate(Unit,Name/Arity))
            )
        ]),
        fail
    ;   !
    ).

    clause_head(A :- _, Z) :- !, A = Z.
    clause_head(A, Z) :- !, A = Z.

    assertz_once(A) :-
        A -> true ; assertz(A).

% -------------------- annotation

annotate_object(Obj, Ann) :-
    assertz(object_annotation(Obj,Ann)).

term_object(Context, Head:-_, Object) :- !,
    Object = file_predicate(File,Name/Arity),
    context_unit(Context,File),
    functor(Head,Name,Arity).

term_object(Context, Head, Object) :- !,
    Object = file_predicate(File,Name/Arity),
    context_unit(Context,File),
    functor(Head,Name,Arity).

%%  resolve_annref(+File, +AnnRef, -ResolvedRef) is det.

resolve_annref(_, file(F), Z) :- !,
    Z = file(F).

resolve_annref(File, predicate(NameArity), Z) :- !,
    Z = file_predicate(File,NameArity).

resolve_annref(File, Ref, _) :- !,
    throw_error(resolve_annref(File,Ref)).

% -------------------- directive

handle_use_module(Context, Rel, Imports) :-
    context_unit(Context, File),
    absolute_file_name(Rel, Abs, [
        relative_to(File),
        file_type(prolog),
        access(read)
    ]),
    (file_visited(Abs)
    ->  true
    ;   assertz(file_visited(Abs)),
        context_module(Context, Current),
        use_module_4(Current, Rel, Abs, Mod),
        assertz(unit_module(Abs,Mod)),
        assertz(unit_import_list(File,module(Mod),Imports))
    ).

handle_import(_, _, Imports) :-
    \+ is_list(Imports), !,
    throw_error(invalid_import_list(Imports)).

handle_import(Unit, file(Rel), Imports) :- !,
    absolute_file_name(Rel, Abs, [relative_to(Unit)]),
    assertz(unit_import_list(Unit,file(Abs),Imports)),
    my_consult(Abs).

handle_import(Unit, system, Imports) :- !,
    assertz(unit_import_list(Unit,system,Imports)).

handle_import(_, Src, _) :- !, type_error(import_source, Src).

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


load_this_file :-
    prolog_load_context(file, File)
    ->  my_consult(File, ['$no_link'])
    ;   throw_error(must_be_called_from_directive).

% Error.
:- initialization(load_this_file, now).
