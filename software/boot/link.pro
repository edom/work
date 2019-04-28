:- annotate(file,[
    purpose-"linking, renaming, name resolution"
]).

linkcon(Unit, Origin, Head, linkcon(Unit,Origin,Head)). % linking context
linkcon_unit(A,B) :- arg(1,A,B). % the unit that includes the include file
linkcon_origin(A,B) :- arg(2,A,B). % the include file that defines the clause
linkcon_head(A,B) :- arg(3,A,B). % head of clause being linked

link_clause(Unit, Origin, A:-B, Y:-Z) :- !,
    A = Y,
    linkcon(Unit, Origin, A, Con),
    link_goal(Con, B, Z).

link_clause(_, _, A, Z) :- !,
    A = Z.

    /*
    ,/2 and ;/2 are actually predicates defined in the "system" module,
    with proper meta-predicate declarations,
    so link_goal/3 does not have to handle them specially:

        predicate_property((_,_), meta_predicate((0,0))).
        predicate_property((_;_), meta_predicate((0;0))).
        predicate_property((_->_), meta_predicate((0->0))).
    */

    :- annotate([
        purpose = "in a unit, resolve a goal to the module-qualified goal"
        , example = link_goal(
            _LinkCon,
            call(hello),
            '@'(system:call(genmod1_unit:hello), genmod1_unit)
        )
        , todo(production, "decorate goal arguments according to meta-predicate declarations")
    ]).

    link_goal(_, Var, _) :- var(Var), !, instantiation_error(Var).
        % The user should wrap the variable in a call/1.

    link_goal(_, Module:Goal, Z) :- !, Z = Module:Goal.

    link_goal(Con, Goal, Qualified) :-
        linkcon_unit(Con, Unit),
        goal_pred(Goal, Pred),
        unit_predicate_module(Unit, Pred, Exporter),
        !,
        link_goal_args(Con, Goal, NewGoal),
        module_goal_qualified(Exporter, NewGoal, Qualified).

    link_goal(Con, Goal, _) :- !,
        linkcon_unit(Con, Unit),
        linkcon_origin(Con, Origin),
        linkcon_head(Con, Head),
        goal_pred(Goal, Pred),
        throw_error(unit_calls_undefined_predicate(Unit,Head,Pred,Origin)).

        link_goal_args(Con, Goal1, Goal2) :-
            functor(Goal1, Name, Arity),
            functor(Goal2, Name, Arity),
            link_goal_args_0(Con, 1, Arity, Goal1, Goal2).

        link_goal_args_0(Con, Index, Arity, Goal1, Goal2) :-
            Index =< Arity ->
                Index1 is Index + 1,
                link_goal_arg(Con, Index, Goal1, Goal2),
                link_goal_args_0(Con, Index1, Arity, Goal1, Goal2)
            ;   true.

            link_goal_arg(Con, Arg, Goal1, Goal2) :-
                linkcon_unit(Con, Unit),
                goal_arg_meta(Goal1, Arg, Meta),
                arg(Arg, Goal1, Old),
                arg(Arg, Goal2, New),
                (   integer(Meta) ->
                        (   var(Old) ->
                                % A goal can have multiple qualifiers.
                                % Such goal looks like a:b:c.
                                % The innermost qualifier is used.
                                % http://www.swi-prolog.org/pldoc/doc_for?object=(meta_predicate)/1
                                get_unit_module_or(Unit, Module, throw),
                                New = Module:Old
                        ;   link_goal(Con, Old, New)
                        )
                ;   Meta = ':' ->
                        get_unit_module_or(Unit, Module, throw),
                        New = Module:Old
                ;   New = Old
                ).

        % If the Prolog implementation has a module system:
        module_goal_qualified(M, G, M:G).

        /*
        % If the Prolog implementation does not have a module system:
        module_goal_qualified(M, G, MG) :-
            G =.. [GName|Args],
            atomic_list_concat([M,'_',GName], MGName),
            MG =.. [MGName|Args].
        */

    :- annotate([
        meaning = "the Arg-th argument of Goal is module-sensitive"
    ]).

%%  unit_predicate_module(+Unit, +Predicate, -Module) is semidet.

:- annotate([
    purpose = "compute the module of an unqualified predicate in a unit"
    , example = unit_predicate_module("/unit.pro", call/1, system)
    , example = unit_predicate_module("/unit.pro", foo:bar/0, foo)
]).

unit_predicate_module(_, M:_/_, Module) :- !,
    Module = M.

unit_predicate_module(Unit, Pred, Module) :-
    unit_predicate(Unit, Pred), !,
    get_unit_module_or(Unit, Module, throw).

unit_predicate_module(Unit, Pred, Module) :-
    unit_import(Unit, user, Pred), !,
    Module = user.

unit_predicate_module(Unit, Pred, Module) :-
    unit_import(Unit, system, Pred), !,
    Module = system.

unit_predicate_module(Unit, Pred, Module) :-
    unit_import(Unit, module(Module), Pred), !.

unit_predicate_module(Unit, Pred, Module) :-
    unit_import(Unit, unit(FE), Pred),
    unit_module(FE, Module).
