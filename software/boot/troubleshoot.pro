/*
\QuestionAnswer{How do I find translation errors?}{
    list/1 the unit.
}
*/

:- section("troubleshoot").

    :- section("user interface").

        :- annotate([
            purpose = 'similar to call/1 except for module resolution'
            , example = my_call(unit("unit.pro"):hello)
        ]).
        my_call(E:G) :-
            eval_mod_exp(E, M),
            module_goal_qualified(M, G, Q),
            call(Q).

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

        list(Name) :- atom(Name), !, list(Name/_).

        list(Name/Arity) :- !,
            forall((
                file_term(File, Index, Term),
                term_clause(Term, Clause),
                clause_head(Clause, Head),
                functor(Head, Name, Arity)
                ),(
                format("% term in file ~w#~w~n", [File,Index]),
                portray_clause(Term),
                format("% clause in file ~w#~w~n", [File,Index]),
                portray_clause(Clause)
            )),
            forall((
                unit_term(Unit, Index, Term, Origin),
                term_clause(Term, Clause),
                clause_head(Clause, Head),
                functor(Head, Name, Arity)
                ),(
                format("% clause in unit ~w#~w~n", [Unit,Index]),
                portray_clause(Clause),
                format("% linked clause in unit ~w#~w~n", [Unit,Index]),
                (link_clause(Unit, Origin, Clause, Linked)
                ->  portray_clause(Linked)
                ;   format("% linking failed", [])
                )
            )).

        list(unit(Reln)) :- !,
            get_unit(Reln, Unit),
            forall(unit_clause_linked(Unit, _, Cla, _),
                portray_clause(Cla)
            ).

            get_unit(R, A) :-
                must_be(ground, R),
                absolute_file_name(R, A),
                (unit(A) -> true ; throw_error(get_unit(R))).

    :- end_section.

:- end_section.

:- section("messages").

    my_location(Unit, Head, Orig) -->
        {origin_file(Orig,File)},
        [
            "In unit          : ~w"-[Unit],nl,
            "In included file : ~w"-[File],nl,
            "In a clause for  : ~w"-[Head],nl
        ].

    % link.pro

    my_message(unit_calls_undefined_predicate(Unit,Head,Pred,Orig)) -->
        my_location(Unit, Head, Orig),
        [
            "The problem      : Undefined predicate: ~p"-[Pred],nl,
            "Solutions:",nl,
            "- If you want to import ~p from somewhere else:"-[Pred],nl,
            "  - Add an import like ~p in that file."-[:-import(somewhere,[Pred])],nl,
            "- If you believe that ~p is builtin:"-[Pred],nl,
            "  - Add a default_import/2 clause in import.pro for ~p."-[Pred],nl,
            "- If you feel that this should not happen:",nl,
            "  - Check for mistakes in spelling, argument counts, and parentheses.",nl,
            "  - Have you defined the predicate?",nl,
            "  - If you believe that this is a programming error in the loader:",nl,
            "    - Call debug/0, and then call ~p, and then"-[list(Pred)],nl,
            "      send the stack trace to the maintainer."
        ].

    % check.pro

    my_message(unit_has_variable_goal_in_clause_body(Unit,Head,Goal,Orig)) -->
        my_location(Unit, Head, Orig),
        [
            "The problem      : Variable in clause body",nl,
            "Solution:",nl,
            "- If you mean to call a goal that is generated at runtime:",nl,
            "  - Wrap the variable in a call/1, like ~p."-[call(Goal)],nl,
            "  - Make sure that you are in control of the possible values of the variable.",nl,
            "    Usually we don't want to pass arbitrary terms to call/1."
        ].

    my_message(unit_imports_unexported_predicate(I,E,P)) --> [
        "In ~p:"-[I],nl,
        "Error: Importing unexported predicate",nl,
        "Predicate: ~p"-[P],nl,
        "Exporter: ~p"-[E]
    ].

    prolog:message(A) --> my_message(A).
    prolog:error_message(A) --> my_message(A).

:- end_section.
