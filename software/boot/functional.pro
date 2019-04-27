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
case(Exp, _) :- var(Exp), !, instantiation_error(Exp).
case(_, Cases) :- var(Cases), !, instantiation_error(Cases).
case(Exp, []) :- !, throw(error(unhandled_case(Exp),_)).
case(_, [Case|_]) :- var(Case), !, instantiation_error(Case).
case(_, [_|Rest]) :- var(Rest), !, instantiation_error(Rest).
case(Exp, [Case|Rest]) :-
    Case = (Exp -> Body)
    ->  call(Body)
    ;   case(Exp, Rest).
