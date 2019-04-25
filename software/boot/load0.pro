:- consult("debug.pro").
:- include("functional.pro").
:- include("genmod.pro").
:- include("read.pro").
:- include("dcg.pro").
:- include("swi.pro").
:- include("check.pro").

:- debug(load). % DEBUG
%:- debug(import_expansion). % DEBUG

% -------------------- database

:- dynamic file_loaded/1. % File
:- dynamic file_module/2. % File, Module
:- dynamic file_term/3. % File, Index, Term
:- dynamic file_import_list/3. % Importer, Exporter, Imports
:- dynamic file_export_list/2. % Exporter, Exports
:- dynamic object_annotation/2. % Object, Annotation

file_clause(F,I,C) :-
    file_term(F,I,T),
    term_clause(T,C).

% --------------------- rewriting

term_clause(:- _, _) :- !, fail.
term_clause(A :- B, Z) :- !, Z = (A :- B).
term_clause(A --> B, Z) :- !, dcg_clause(A --> B, Z).
term_clause(A, Z) :- !, Z = A.

clause_head(A :- _, Z) :- !, A = Z.
clause_head(A, Z) :- !, A = Z.

% -------------------- reference resolution

file_import(Importer,Exporter,Pred) :-
    file_import_list(Importer,Exporter,List),
    member(Pred,List).

file_export(Exporter,Pred) :-
    file_export_list(Exporter,List),
    member(Pred,List).

object_import(file(I),E,P) :-
    file_import(I,E,P).

object_export(system,Pred) :-
    current_predicate(system:Pred).

object_export(user,Pred) :-
    current_predicate(user:Pred).

object_export(module(M),Pred) :-
    module_property(M,exports(Es)),
    member(Pred,Es).

object_export(file(E),Pred) :-
    file_export(E,Pred).

default_import(system,[
    !/0
    , '<'/2
    , '=<'/2
    , '>'/2
    , '>='/2
    , call/1
    , call/2
    , call/3
    , call/4
    , format/2
    , length/2
    , is/2
]).
default_import(user,[
    '$my_phrase'/3
    , append/3
    , member/2
]).

file_defines(F,N/A) :-
    file_clause(F,_,C),
    clause_head(C,H),
    functor(H,N,A).

file_predicate_origin(F,P,local) :-
    file_defines(F,P).

file_predicate_origin(F,P,module(system)) :-
    file_import(F,system,P).

file_predicate_origin(F,P,module(user)) :-
    file_import(F,user,P).

file_predicate_origin(F,P,module(E)) :-
    file_import(F,module(E),P).

file_predicate_origin(F,P,module(E)) :-
    file_import(F,file(FE),P),
    file_module(FE,E).

% -------------------- linking

file_clause_linked(F, A:-B, Y:-Z) :- !, A=Y, file_goal_linked(F,B,Z).
file_clause_linked(_, A, Z) :- !, A=Z.

file_goal_linked(F,(A,B),(Y,Z)) :- !, file_goal_linked(F,A,Y), file_goal_linked(F,B,Z).
file_goal_linked(F,(A;B),(Y;Z)) :- !, file_goal_linked(F,A,Y), file_goal_linked(F,B,Z).

file_goal_linked(F,A,Z) :-
    functor(A,Name,Arity),
    file_predicate_origin(F,Name/Arity,local), !,
    Z = A.

file_goal_linked(F,A,Z) :-
    functor(A,Name,Arity),
    file_predicate_origin(F,Name/Arity,module(M)),
    file_module(F,I),
    !,
    Z = '@'(M:A,I).

file_goal_linked(F,A,_) :- !,
    throw_error(cannot_resolve_goal(F,A)).

prolog:error_message(cannot_resolve_goal(F,G)) -->
    {functor(G,N,A)},
    [
        "In ~w:"-[F],nl,
        "Cannot resolve goal: ~w"-[G],nl,
        "because that file does not import that predicate.",nl,
        "Possible fix: Add something like ~w in that file."-[:-import(somewhere,[N/A])]
    ].

obj_ann_1(O,A) :-
    object_annotation(O,As),
    member(A,As).

obj_tag(O,T) :-
    obj_ann_1(O,tags-Ts),
    member(T,Ts).

% -------------------- context

context(A) :- functor(A,context,2).

context_file(A,B) :- context(A), arg(1,A,B).
context_module(A,B) :- context(A), arg(2,A,B).

context_new(File, Module, Context) :-
    context(Context),
    context_file(Context, File),
    context_module(Context, Module).

% -------------------- consult

% This may leak resource, but how portable is setup_call_cleanup/3?
my_consult(Path) :-
    absolute_file_name(Path, File),
    (file_loaded(File)
    ->  true
    ;   assertz(file_loaded(File)),
        generate_module_name(File, Module),
        initialize_module(Module),
        debug(load, "my_consult: loading file ~w into ~w", [File,Module]),
        assertz(file_module(File,Module)),
        assert_default_imports(File),
        context_new(File, Module, Context),
        open(File, read, Stream, [eof_action(eof_code)]),
        read_stream(Stream, Terms),
        forall(nth1(Index,Terms,Term),
            assertz(file_term(File,Index,Term))),
        handle_terms(Context),
        close(Stream),
        my_check,
        (error(_)
        ->  true
        ;   actuate(File))
    ).

assert_default_imports(File) :-
    forall(default_import(Src,Imports),
        assertz(file_import_list(File,Src,Imports))
    ).

actuate(File) :-
    repeat, (
        file_term(File, _, Clause),
        file_clause_linked(File, Clause, Linked),
        file_module(File, Module),
        '@'(system:assert(Linked), Module)
    ;   !
    ).

% -------------------- load

handle_terms(Context) :-
    context_file(Context, File),
    repeat, (
        file_term(File, Index, Term),
        case(Term,[
            (:- annotate(Ann)) -> (
                Index1 is Index+1,
                file_term(File, Index1, Term1),
                term_object(Context, Term1, Object),
                annotate_object(Object, Ann)
            ),
            % section/1 tries to help people with non-folding text editors
            (:- section(_)) ->
                true,
            (:- Dir) ->
                handle_directive(Context, Dir),
            _ -> (
                term_clause(Term, Clause),
                writeln(Clause),nl % DEBUG
            )
        ]),
        fail
    ;   !
    ).

% -------------------- annotation

annotate_object(Obj, Ann) :-
    assertz(object_annotation(Obj,Ann)).

term_object(Context, Head:-_, Object) :- !,
    Object = file_predicate(File,Name/Arity),
    context_file(Context,File),
    functor(Head,Name,Arity).

term_object(Context, Head, Object) :- !,
    Object = file_predicate(File,Name/Arity),
    context_file(Context,File),
    functor(Head,Name,Arity).

annref_resolve(C, file, Z) :- !,
    Z = file(File),
    context_file(C, File).

annref_resolve(C, predicate(NameArity), Z) :- !,
    Z = file_predicate(File,NameArity),
    context_file(C, File).

annref_resolve(C, Ref, _) :- !,
    throw_read_error(C, invalid_annotation_reference(Ref)).

% -------------------- directive

handle_directive(Context, Dir) :-
    context_file(Context, File),
    case(Dir,[
        annotate(Ref,Ann) -> (
            annref_resolve(Context, Ref, Obj),
            annotate_object(Obj, Ann)
        ),
        exports(Exports) ->
            assertz(file_export_list(File,Exports)),
        import(Src,Imports) ->
            handle_import(File, Src, Imports),
        use_module(Rel,Imports) ->
            handle_use_module(Context,Rel,Imports),
        _ ->
            throw_read_error(Context, unknown_directive(Dir))
    ]).

handle_use_module(Context, Rel, Imports) :-
    context_file(Context, File),
    absolute_file_name(Rel, Abs, [
        relative_to(File),
        file_type(prolog),
        access(read)
    ]),
    (file_loaded(Abs)
    ->  true
    ;   assertz(file_loaded(Abs)),
        context_module(Context, Current),
        '@'(system:use_module(Abs), Current),
        (source_file_property(Abs, module(Mod))
        ->  assertz(file_module(Abs,Mod)),
            assertz(file_import_list(File,module(Mod),Imports))
        ;   throw(error(impossible(use_module(Rel,Imports),Abs),_))
        )
    ).

handle_import(_, _, Imports) :-
    \+ is_list(Imports), !,
    throw_error(invalid_import_list(Imports)).

handle_import(File, file(Rel), Imports) :- !,
    absolute_file_name(Rel, Abs, [relative_to(File)]),
    assertz(file_import_list(File,file(Abs),Imports)),
    my_consult(Abs).

handle_import(File, system, Imports) :- !,
    assertz(file_import_list(File,system,Imports)).

handle_import(_, Src, _) :- !, type_error(import_source, Src).

% -------------------- throw

throw_error(Term) :-
    throw(error(Term,_)).

throw_read_error(Context, Error) :-
    throw_error(read_error(Context,Error)).

% -------------------- DEBUG test

test :-
    repeat, (
        file_term(F,I,T),
        format("~`=t~30| ~w#~w~n",[F,I]),

        format("~`-t~30| read/1~n",[]),
        portray_clause(T),

        term_clause(T,C),
        format("~`-t~30| term_clause/2~n",[]),
        portray_clause(C),

        file_clause_linked(F,C,L),
        format("~`-t~30| file_clause_linked/3~n",[]),
        portray_clause(L),

        fail
    ;   !
    ).

% -------------------- load files specified on command-line arguments

load_from_argv :-
    current_prolog_flag(argv, Argv),
    forall(
        member(Arg, Argv),
        my_consult(Arg)
    ).

:- initialization(load_from_argv).
