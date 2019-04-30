:- annotate(file,[
    purpose-"linking, renaming, name resolution"
]).

linkcon(Unit, Origin, Head, linkcon(Unit,Origin,Head)). % linking context
linkcon_unit(A,B) :- arg(1,A,B). % the unit that includes the include file
linkcon_origin(A,B) :- arg(2,A,B). % the include file that defines the clause
linkcon_head(A,B) :- arg(3,A,B). % head of clause being linked

link_clause(_, _, A, _) :- var(A), !, instantiation_error(A).

link_clause(Unit, Origin, A:-B, Z) :- !,
    Z = (A:-Y),
    linkcon(Unit, Origin, A, Con),
    link_goal(Con, B, Y).

link_clause(_, _, A, Z) :- !, A = Z.

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

    /*
        These predicates are actually defined in the "system" module:

            - ,/2
            - ;/2
            - ->/2
            - \+/1

        They actually have proper meta-predicate declarations,
        which can be queried like this:

            predicate_property((_,_), meta_predicate((0,0))).

        However, we do handle them specially in order to make the generated output readable;
        otherwise (A,B) would be linked as system:(A,B).

        We assume that those predicates are not redefined.
    */

    link_goal(Con, (A,B), Z) :- !, Z = (X,Y), link_goal(Con, A, X), link_goal(Con, B, Y).
    link_goal(Con, (A;B), Z) :- !, Z = (X;Y), link_goal(Con, A, X), link_goal(Con, B, Y).
    link_goal(Con, (A->B), Z) :- !, Z = (X->Y), link_goal(Con, A, X), link_goal(Con, B, Y).
    link_goal(Con, (\+A), Z) :- !, Z = (\+Y), link_goal(Con, A, Y).

    link_goal(Con, Goal, Qualified) :-
        linkcon_unit(Con, Unit),
        goal_pred(Goal, Pred),
        unit_predref_qualified(Unit, Pred, Exporter:_), % FIXME
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
                ;   Meta = '^' ->
                        link_goal_ex(Con, Old, New)
                ;   Meta = ':' ->
                        get_unit_module_or(Unit, Module, throw),
                        New = Module:Old
                ;   New = Old
                ).

                link_goal_ex(Con, A, Z) :- var(A), !,
                    linkcon_unit(Con, Unit),
                    get_unit_module_or(Unit, Module, throw),
                    Z = Module:A.

                link_goal_ex(Con, A^B, Z) :- !,
                    Z = A^Y,
                    link_goal_ex(Con, B, Y).

                link_goal_ex(Con, Goal, Z) :- !,
                    link_goal(Con, Goal, Z).

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

%%  unit_predref_qualified(+Unit, +NameArity, -Qualified) is semidet.

:- annotate([
    purpose = "qualify a predicate reference found in a unit"
    , example = unit_predref_qualified("/unit.pro", call/1, system:call/1)
    , example = unit_predref_qualified("/unit.pro", foo:bar/0, foo:bar/0)
]).

unit_predref_qualified(Unit, Ref, Qual) :-
    (var(Unit) -> instantiation_error(Unit) ; true),
    (var(Ref) -> instantiation_error(Ref) ; true),
    (   % The reference is already qualified.
        Ref = _:_/_
    ->  Qual = Ref

    ;   % The reference refers to a predicate defined inside the unit itself.
        unit_predicate(Unit, Ref)
    ->  get_unit_module(Unit, Module),
        Qual = Module:Ref

    ;   % The reference refers to a predicate imported into the unit from another unit.
        unit_import(Unit, Source, Ref, Actual)
    ->  (   member(Source, [system,user])
        ->  Qual = Source:Actual
        ;   Source = module(Module)
        ->  Qual = Module:Actual
        ;   Source = unit(Exporter)
        ->  unit_module(Exporter, Module), !,
            Qual = Module:Actual
        )
    ).

    unit_import(Importer, Exporter, Ref, Actual) :-
        unit_import_list(Importer, Exporter, List),
        member(Item, List),
        item_alias_actual(Item, Ref, Actual).

        item_alias_actual(Item, Ref, Actual) :-
            (   Item = (Name/Arity as Alias)
            ->  Ref = Alias/Arity,
                Actual = Name/Arity

            ;   Item = Name/Arity
            ->  Ref = Name/Arity,
                Actual = Ref

            ;   type_error(import_item, Item)
            ).

    unit_import(Importer, Exporter, Ref) :-
        unit_import(Importer, Exporter, Ref, _).

    object_import(unit(I),E,P) :-
        unit_import(I,E,P).
