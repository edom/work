:- consult("debug.pro").
:- include("functional.pro").
:- include("genmod.pro").

:- debug(load). % DEBUG
%:- debug(import_expansion). % DEBUG

:- dynamic file_module/2.
:- dynamic file_clause/2.
:- dynamic file_import_list/3.
:- dynamic file_export_list/2.
:- dynamic file_export_unchecked/1.
:- dynamic object_annotation/2.

builtin(format/2).

file_import(Importer,Exporter,Pred) :-
    file_import_list(Importer,Exporter,List),
    member(Pred,List).

file_export(Exporter,Pred) :-
    file_export_list(Exporter,List),
    member(Pred,List).

clause_linked(F, H:-B, H:-L) :- !,
    goal_linked(F,B,L).

goal_linked(F,(A,B),(Y,Z)) :- !,
    goal_linked(F,A,Y),
    goal_linked(F,B,Z).

goal_linked(F,(A;B),(Y;Z)) :- !,
    goal_linked(F,A,Y),
    goal_linked(F,B,Z).

goal_linked(_,A,Z) :-
    functor(A,Name,Arity),
    builtin(Name/Arity), !,
    Z = A.

goal_linked(F,A,Z) :-
    functor(A,Name,Arity),
    file_defines(F,Name/Arity),
    file_module(F,M), !,
    Z = M:A.

goal_linked(F,A,Z) :-
    Z = '@'(M:A,I),
    functor(A,Name,Arity),
    file_import(F,E,Name/Arity),
    file_module(F,I),
    file_module(E,M),
    !.

goal_linked(F,A,_) :-
    functor(A,Name,Arity),
    throw(error(file_mentions_predicate_of_unknown_origin(F,Name/Arity),_)).

file_defines(F,N/A) :-
    file_clause(F,C),
    clause_head(C,H),
    functor(H,N,A).

clause_head(A :- _, B) :- !, A = B.
clause_head(A, A).

obj_ann_1(O,A) :-
    object_annotation(O,As),
    member(A,As).

obj_tag(O,T) :-
    obj_ann_1(O,tags-Ts),
    member(T,Ts).

% -------------------- definite clause grammar

clause_dcg(A --> B, Y :- Z) :- !,
    add_args(A,[I,J],Y),
    dcg_body(B,I,J,Z).

add_args(A,Args,Z) :-
    A =.. List1,
    append(List1,Args,List2),
    Z =.. List2.

dcg_body(A,_,_,_) :- var(A), !, instantiation_error(A).

dcg_body((A,B),I,K,Z) :- !,
    Z = (X,Y),
    dcg_body(A,I,J,X),
    dcg_body(B,J,K,Y).

dcg_body((A;B),I,K,Z) :- !,
    Z = (X;Y),
    dcg_body(A,I,K,X),
    dcg_body(B,I,K,Y).

dcg_body({A},_,_,Z) :- !,
    Z = A.

dcg_body(A,I,K,Z) :- is_list(A), !,
    Z = append(A,K,I).

dcg_body(A,I,J,Z) :- string(A), !,
    string_codes(A,B),
    dcg_body(B,I,J,Z).

dcg_body(A,I,K,Z) :- !,
    add_args(A,[I,K],Z).

% -------------------- context

context(A) :- functor(A,context,3).

context_file(A,B) :- context(A), arg(1,A,B).
context_module(A,B) :- context(A), arg(2,A,B).
context_annotation(A,B) :- context(A), arg(3,A,B). % for next read item
nb_context_set_annotation(A,B) :- context(A), nb_setarg(3,A,B).

% -------------------- load

my_consult(Path) :-
    absolute_file_name(Path, File),
    (file_module(File, _)
    ->  true    % already loaded
    ;   generate_module_name(File, Mod),
        debug(load, "abs_load_once: loading file ~w into ~w", [File,Mod]),
        assertz(file_module(File,Mod)),
        context(Context),
        context_file(Context, File),
        context_annotation(Context, none),
        context_module(Context, Mod),
        setup_call_cleanup(
            open(Path, read, Stream, [eof_action(eof_code)]),
            consult_stream(Context, Stream),
            close(Stream)
        )
    ),
    my_check,
    (error(_)
    ->  true
    ;   actuate(File)).

actuate(File) :-
    file_clause(File, Clause),
    clause_linked(File, Clause, Linked),
    file_module(File, Module),
    '@'(system:assert(Linked), Module).



read_term_1(_, Stream, OptTerm) :-
    read_term(Stream, Term, [syntax_errors(error)]), !,
    (Term == end_of_file, stream_property(Stream, end_of_stream(past))
    ->  OptTerm = none
    ;   OptTerm = some(Term)).

read_term_1(Context, _, _) :-
    throw_read_error(Context, syntax_error).


consult_stream(Context, Stream) :-
    read_term_1(Context, Stream, OptTerm),
    case(OptTerm,[
        some(Term) -> (
            handle_term(Context, Term),
            consult_stream(Context, Stream)
        ),
        none ->
            true
    ]).

handle_term(Context, Term) :-
    case(Term,[
        (:- Dir) ->
            handle_directive(Context, Dir),
        _ -> (
            annotate_term(Context, Term),
            context_file(Context, File),
            assertz(file_clause(File, Term))
        )
    ]).

annotate_term(Context, _) :- context_annotation(Context, none), !.
annotate_term(Context, Term) :-
    context_annotation(Context, Ann), !,
    term_object(Context, Term, Obj),
    assertz(object_annotation(Obj,Ann)),
    nb_context_set_annotation(Context,none).

term_object(Context, Head:-_, Object) :- !,
    Object = file_predicate(File,Name/Arity),
    context_file(Context,File),
    functor(Head,Name,Arity).

term_object(Context, Head, Object) :- !,
    Object = file_predicate(File,Name/Arity),
    context_file(Context,File),
    functor(Head,Name,Arity).

handle_directive(Context, Dir) :-
    context_file(Context, File),
    case(Dir,[
        annotate(Ann) ->
            nb_context_set_annotation(Context, Ann),
        annotate(Ref,Ann) ->
            handle_annotate(Context, Ref, Ann),
        section(_) -> % to help people with non-folding text editors
            true,
        exports(Exports) ->
            assertz(file_export_list(File,Exports)),
        import(Src,Imports) ->
            handle_import(Context, Src, Imports),
        use_module(Rel,Imports) -> (
                context_module(Context, Current),
                Opts = [
                    relative_to(File),
                    file_type(prolog),
                    access(read)
                ],
                absolute_file_name(Rel, Abs, Opts),
                assertz(file_import_list(File, Abs, Imports)),
                assertz(file_export_unchecked(Abs)),
                '@'(system:use_module(Abs), Current),
                (source_file_property(Abs, module(Mod))
                ->  assertz(file_module(Abs,Mod))
                ;   throw(error(impossible(use_module(Rel,Imports),Abs),_))
                )
            ),
        _ ->
            throw_read_error(Context, unknown_directive(Dir))
    ]).

handle_import(Context, _, Imports) :-
    \+ is_list(Imports), !,
    throw_read_error(Context, invalid_import_list(Imports)).

handle_import(Context, file(Rel), Imports) :- !,
    context_file(Context, File),
    absolute_file_name(Rel, Abs, [relative_to(File)]),
    assertz(file_import_list(File, Abs, Imports)),
    my_consult(Abs).

handle_import(_, Src, _) :- !, type_error(import_source, Src).

handle_annotate(Context, Ref, Ann) :-
    annref_resolve(Context, Ref, Obj),
    assertz(object_annotation(Obj,Ann)).

annref_resolve(C, file, Z) :- !,
    Z = file(File),
    context_file(C, File).
annref_resolve(C, predicate(NameArity), Z) :- !,
    Z = file_predicate(File,NameArity),
    context_file(C, File).
annref_resolve(C, Ref, _) :- !,
    throw_read_error(C, invalid_annotation_reference(Ref)).

throw_read_error(Context, Error) :-
    throw(error(read_error(Context,Error),_)).

% -------------------- check

my_check :-
    forall(error(E), print_message(error,E)).

error(importing_unexported_predicate(I,E,P)) :-
    file_import(I,E,P),
    \+ file_export_unchecked(E),
    \+ file_export(E,P).

% -------------------- load files specified on command-line arguments

:-  current_prolog_flag(argv, Argv),
    forall(
        member(Arg, Argv),
        my_consult(Arg)
    ).
