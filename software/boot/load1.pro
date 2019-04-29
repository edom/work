:- annotate(file,[
    purpose-"enable Prolog programming in the large"
    , tags-[
        architecture
    ]
]).

:- export([
    my_consult/1
]).

:- import(system,[
    nb_current/2
    , nb_setval/2
]).

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
    , todo(production, "remove $no_link hack")
]).
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
        %   The '$no_link' option is an internal hack used by my_consult/1
        %   to load its own source code without falling into an infinite loop.
        %
        %   A better solution is conditional compilation.
        (member('$no_link', Opts)
        ->  true
        ;   (unit_error(Unit, _)
            ->  print_unit_errors(Unit)
            ;   link_unit(Context),
                assertz(unit_linked(Unit))
            )
        )
    ).

    :- annotate([
        purpose = "collect annotations, expand includes, and populate unit_term/4"
        , todo(production, "limit recursion depth")
    ]).
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
                (:- dynamic(Name/Arity)) -> (
                    assertz_once(file_predicate(File, Name/Arity))
                ),
                (:- dynamic(A)) -> (
                    throw_error(syntax_error(Term))
                ),
                (:- include(Rel)) -> (
                    read_file_abs(Rel, Abs, [relative_to(File)]),
                    assertz_once(file_include(File, Abs)),
                    do_unit_include(Context, Abs)
                ),
                _ -> (
                    context_unit(Context, Unit),
                    context_term_count(Context, Count),
                    UIndex is Count+1,
                    nb_set_context_term_count(Context, UIndex),
                    origin_file(Origin, File),
                    (   term_clause(Term, Clause),
                        clause_head(Clause, Head),
                        goal_pred(Head, Pred)
                    ->  assertz_once(file_predicate(File, Pred))
                    ;   true
                    ),
                    assertz(unit_term(Unit,UIndex,Term,Origin))
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

% -------------------- load

:- annotate([
    problem-"dynamic/1 not handled"
    , problem-"initialization/2 not handled"
]).
interpret_directives(Context) :-
    context_unit(Context, Unit),
    forall(unit_term(Unit, _, Term, _),
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
        ])
    ).

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
    nb_current(my_loader_loaded, _)
    ->  true
    ;   nb_setval(my_loader_loaded, true),
        get_prolog_current_file(File),
        my_consult(File).

get_prolog_current_file(File) :-
    prolog_load_context(file, File)
    ->  true
    ;   throw_error(must_be_called_from_directive).

% Error.
:- initialization(load_this_file, now).
