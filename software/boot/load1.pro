:- annotate(file,[
    purpose-"enable Prolog programming in the large"
    , tags-[
        architecture
    ]
]).

:- export([
    my_consult/1
]).

:- use_module(library(gensym),[
    gensym/2
]).
:- use_module(library(listing),[
    portray_clause/1
]).

:- include("functional.pro").
:- include("genmod.pro").
:- include("read.pro").
:- include("clause.pro").
:- include("swi.pro").
:- include("check.pro").
:- include("import.pro").
:- include("link.pro").


:- section("database").

    :- annotate(relation(unit_module/2),[cardinality=1:1]).

    :- dynamic file_visited/1. % File
    :- dynamic file_term/3. % File, Index, Term
    :- dynamic unit/1. % File
    :- dynamic unit_linked/1. % File
    :- dynamic unit_module/2. % File, Module
    :- dynamic unit_term/3. % File, Index, Term
    :- dynamic unit_import_list/3. % Importer, Exporter, Imports
    :- dynamic unit_export_list/2. % Exporter, Exports
    :- dynamic object_annotation/2. % Object, Annotation

    file_predicate(File, Pred) :-
        setof(P, file_pred(File,P,[]), Ps),
        member(Pred, Ps).

        file_pred(F, N/A, Opts) :-
            must_be(ground, Opts),
            file_term(F, _, T),
            case(T,[
                (:- dynamic(N/A)) -> true,
                (:- include(That)) -> (
                    member(follow_includes(true), Opts)
                    ->  file_pred(That, N/A, Opts)
                    ;   true
                ),
                _ -> (
                    term_clause(T, C),
                    clause_head(C, H),
                    functor(H, N, A)
                )
            ]).

    unit_predicate(Unit, Pred) :-
        unit(Unit),
        setof(P, file_pred(Unit,P,[follow_includes(true)]), Ps),
        member(Pred, Ps).

    get_unit_module_or(Unit, Module, Alt) :-
        unit_module(Unit, Module)
        ->  true
        ;   case(Alt,[
                generate -> (
                    generate_module_name(Unit, Module),
                    assertz(unit_module(Unit,Module)),
                    initialize_module(Module)
                ),

                throw ->
                    throw_error(unit_module_failed(Unit)),

                fail ->
                    fail,

                _ ->
                    throw_error(invalid_alternative(Alt))
            ]).

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
    assertz_once(unit(Unit)),
    (unit_linked(Unit)
    ->  true
    ;   get_unit_module_or(Unit, Module, generate),
        debug(load_unit_into_module, "my_consult: loading unit ~w into ~w", [Unit,Module]),
        context_new(Unit, Module, Context),
        do_unit_include(Context, Unit),
        interpret_directives(Context),
        (member('$no_link', Opts)
        ->  true
        ;   (unit_error(Unit, _)
            ->  forall(unit_error(Unit, Error),
                    print_message(error, unit_error(Unit,Error))
                )
            ;   actuate_unit(Context),
                assertz(unit_linked(Unit))
            )
        )
    ).

    assertz_once(G) :-
        must_be(ground, G),
        call(G) -> true ; assertz(G).


    :- annotate([purpose-"collect annotations, expand includes, and populate unit_term/3"]).
    do_unit_include(Context, File) :-
        forall(file_term(File, FIndex, Term), (
            case(Term,[
                /*
                    annotate(file,Ann) means annotate the file that contains the source code.
                    That file is the file that contains the bytes that represent the textual source code.

                    Many files can be loaded into the same module by include/1.
                */
                (:- annotate(file,Ann)) ->
                    annotate_object(file(File), Ann),

                % TODO resolve N/A against imports
                (:- annotate(predicate(N/A),Ann)) ->
                    annotate_object(file_predicate(File,N/A), Ann),

                % relation/1 is synonym for predicate/1 but hints finiteness and multidirectionality
                (:- annotate(relation(N/A),Ann)) ->
                    annotate_object(file_predicate(File,N/A), Ann),

                (:- annotate(Ref,_)) ->
                    throw_error(invalid_annotation_ref(Ref)),

                (:- annotate(Ann)) -> (
                    FIndex1 is FIndex+1,
                    file_term(File, FIndex1, Term1),
                    term_object(File, Term1, Object),
                    annotate_object(Object, Ann)
                ),

                (:- include(Rel)) -> (
                    read_file_abs(Rel, Abs, [relative_to(File)]),
                    do_unit_include(Context, Abs)
                ),
                _ -> (
                    context_unit(Context, Unit),
                    context_term_count(Context, Count),
                    UIndex is Count+1,
                    nb_set_context_term_count(Context, UIndex),
                    assertz(unit_term(Unit,UIndex,Term))
                )
            ])
        )).

    annotate_object(Obj, Ann) :-
        assertz(object_annotation(Obj,Ann)).

    term_object(File, Head:-_, Object) :- !,
        Object = file_predicate(File,Name/Arity),
        functor(Head,Name,Arity).

    term_object(File, Head, Object) :- !,
        Object = file_predicate(File,Name/Arity),
        functor(Head,Name,Arity).

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
        read_stream(File, Stream, 1, _),
        close(Stream)
    ).

    read_stream(File, Stream, Index, Index2) :-
        read_term_1(Stream, OptTerm),
        case(OptTerm,[
            some(Term) -> (
                assertz(file_term(File,Index,Term)),
                Index1 is Index+1,
                read_stream(File, Stream, Index1, Index2)
            ),
            none -> (
                Index2 = Index
            )
        ]).

actuate_unit(Context) :-
    context_unit(Context, Unit),
    context_module(Context, Module),
    forall(unit_clause_linked(Unit, _, Linked),
        assert_2(Module, Linked)
    ).

    unit_clause(Unit, Index, Clause) :-
        must_be(ground, Unit),
        findall(C, (
            unit_term(Unit, Index, Term),
            term_clause(Term, C)
        ), Clauses),
        nth1(Index, Clauses, Clause).

    unit_clause_linked(Unit, Index, Linked) :-
        unit_clause(Unit, Index, Clause),
        link_unit_clause(Unit, Clause, Linked).

% -------------------- load

:- annotate([
    problem-"dynamic/1 not handled"
    , problem-"initialization/2 not handled"
]).
interpret_directives(Context) :-
    context_unit(Context, Unit),
    repeat, (
        unit_term(Unit, _, Term),
        case(Term,[
            (:- Dir) -> case(Dir,[
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
            _ ->
                true
        ]),
        fail
    ;   !
    ).

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

handle_import(_, _, Imports) :-
    \+ is_list(Imports), !,
    throw_error(invalid_import_list(Imports)).

handle_import(Unit, file(Rel), Imports) :- !,
    absolute_file_name(Rel, Abs, [relative_to(Unit)]),
    assertz(unit_import_list(Unit,unit(Abs),Imports)),
    my_consult(Abs).

handle_import(Unit, system, Imports) :- !,
    assertz(unit_import_list(Unit,system,Imports)).

handle_import(Unit, user, Imports) :- !,
    assertz(unit_import_list(Unit,user,Imports)).

handle_import(_, Src, _) :- !, type_error(import_source, Src).

% -------------------- throw

throw_error(Term) :-
    throw(error(Term,_)).

:- annotate([superseded_by = throw_file_read_error/2]).
throw_read_error(Context, Error) :-
    throw_error(read_error(Context,Error)).

throw_file_read_error(File, Error) :-
    throw_error(read_error(File,Error)).


:- section("experiment").

my_call(E:G) :-
    eval_mod_exp(E,M),
    call(M:G).

eval_mod_exp(unit(Rel), Mod) :- !,
    must_be(ground, Rel),
    absolute_file_name(Rel, Abs),
    (file_visited(Abs) -> true ; throw_error(file_not_visited(Rel))),
    (unit_linked(Abs) -> true ; throw_error(unit_not_linked(Rel))),
    get_unit_module_or(Abs, Mod, throw).

eval_mod_exp(Exp, _) :-
    type_error(module_expression, Exp).

:- end_section.


:- section("find things and show their sources").

list(A) :- var(A), !,
    writeln("
usage:

    list(Name/Arity)
    list(unit(Path))
        where Path may be relative
").

list(Name/Arity) :- !,
    forall((
        file_term_expanded(File, Index, Term),
        term_clause(Term, Clause),
        clause_head(Clause, Head),
        functor(Head, Name, Arity)
        ), (
        format("% ~w#~w~n", [File,Index]),
        portray_clause(Clause)
    )).

list(unit(Reln)) :- !,
    get_unit(Reln, Unit),
    forall(unit_clause_linked(Unit, _, Cla),
        portray_clause(Cla)
    ).

    get_unit(R, A) :-
        must_be(ground, R),
        absolute_file_name(R, A),
        (unit(A) -> true ; throw_error(get_unit(R))).

:- end_section.


:- section("term expansion and goal expansion").

    my_expand_term(A, Z) :- var(A), !, A = Z.
    my_expand_term(A, Z) :- my_expand_goals(A, Z).

    my_expand_goals(A, Z) :- !, A = Z.

    expand_case(case(Exp,[Pat->Bod|Alt]), Z) :- !,
        Z = (Exp=Pat -> Bod ; ExpAlt),
        expand_case(case(Exp,Alt), ExpAlt).

    expand_case(case(_,[Pat|_]), _) :- !,
        throw(error(invalid_case_element(Pat),_)).

    expand_case(case(Exp,[]), Z) :- !,
        Z = throw(error(unhandled_case(Exp),_)).

    expand_case(case(A,B), _) :- !,
        throw(error(invalid_case(case(A,B)),_)).

    file_term_expanded(F, I, E) :-
        file_term(F, I, T),
        my_expand_term(T, E).

:- end_section.


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
