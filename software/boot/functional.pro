/*
Similar to Haskell/ML case expression,
Coq/Scheme match expression.

Usage:

    case(Exp,[
        Pattern1 -> Goal1,
        ...,
        PatternN -> GoalN,
        _ -> DefaultGoal
    ])

Exp must be nonvar.

Example:

    % (The Option term comes from somewhere else.)
    case(Option,[
        none -> format("none~n",[]),
        some(A) -> format("some ~w~n",[A])
    ])
*/

%   Keep case/2 and expand_case/2 consistent with each other.

:- meta_predicate case(?,:).

case(Exp, _) :- var(Exp), !, instantiation_error(Exp).
case(_, Cases) :- var(Cases), !, instantiation_error(Cases).
case(_, _:Cases) :- var(Cases), !, instantiation_error(Cases).
case(Exp, _:[]) :- !, throw(error(unhandled_case(Exp),_)).
case(_, _:[Case|_]) :- var(Case), !, instantiation_error(Case).
case(_, _:[_|Rest]) :- var(Rest), !, instantiation_error(Rest).
case(Exp, Mod:[(Pat->Body)|Rest]) :- !,
    (   Exp = Pat
    ->  call(Mod:Body)
    ;   case(Exp, Rest)
    ).
case(_, Cases) :- !, type_error(case_bodies, Cases).

expand_case(case(Exp,[Pat->Bod|Alt]), Z) :- !,
    Z = (Exp=Pat -> Bod ; ExpAlt),
    expand_case(case(Exp,Alt), ExpAlt).

expand_case(case(_,[Pat|_]), _) :- !,
    throw(error(invalid_case_element(Pat),_)).

expand_case(case(Exp,[]), Z) :- !,
    Z = throw(error(unhandled_case(Exp),_)).

expand_case(case(A,B), _) :- !,
    throw(error(invalid_case(case(A,B)),_)).
