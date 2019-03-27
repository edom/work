% -------------------- behavior

:- dynamic state/2.

:- op(10,fx,#).
:- op(10,xfx,@).
:- op(100,yfx,++).

interpret(A, Z) :- interpret([], A, Z).

interpret(C, A * B, Z) :- !,
    interpret(C, A, A0),
    interpret(C, B, B0),
    Z is A0 * B0.

interpret(C, A + B, Z) :- !,
    interpret(C, A, A0),
    interpret(C, B, B0),
    Z is A0 + B0.

interpret(C, (A,B), Z) :- !,
    interpret(C, A, _),
    interpret(C, B, Z).

interpret(_, states([]) := [], Z) :- !, Z = unit.
interpret(C, states([A|B]) := [X|Y], Z) :- !,
    interpret(C, state(A) := X, _),
    interpret(C, states(B) := Y, Z).

interpret(C, A ++ B, Z) :- !, interpret(C,A,A0), interpret(C,B,B0), append(A0,B0,Z).

interpret(_, quote(A), Z) :- !, Z = A.

interpret(C, let([],B), Z) :- !, interpret(C, B, Z).
interpret(C, let([Name=Exp|Binds],Body), Z) :- !,
    interpret(C, Exp, Val),
    interpret([Name-Val|C], let(Binds,Body), Z).

interpret(C, $Name, Z) :- !,
    (member(Name-Val, C)
    ->  Z = Val
    ;   existence_error(variable, Name)
    ).

interpret(C, state(S) := E, Z) :- !,
    Z = unit,
    interpret(C, E, V),
    retractall(state(S,_)),
    assertz(state(S,V)).

interpret(_, [] := [], Z) :- !, Z = unit.
interpret(C, [A|B] := [X|Y], Z) :- !,
    interpret(C, A := X, _),
    interpret(C, B := Y, Z).

interpret(_, state(S), Z) :- state(S, V), !, Z = V.
interpret(_, state(S), _) :- !, type_error(state, state(S)).
interpret(_, format(A,B), Z) :- !, Z = unit, format(A,B).
interpret(_, read, Z) :- !, read(Z).
interpret(_, [], Z) :- !, Z = [].
interpret(C, [A|B], Z) :- !, Z = [A0|B0], interpret(C, A, A0), interpret(C, B, B0).
interpret(C, context, Z) :- !, Z = C.
interpret(_, A, Z) :- number(A), !, Z = A.
interpret(_, E, _) :- !, type_error(expression, E).

% -------------------- structure

class(person).
class_property(person, name).
class_property(person, age).

% database(?Class, ?Id, ?Object) is nondet.
:- dynamic database/2.

:- nb_setval(database_id, 0).

generate_id(Id) :-
    nb_getval(database_id, Id),
    Id1 is Id + 1,
    nb_setval(database_id, Id1).

insert(Class, Object) :-
    generate_id(Id),
    assertz(database(Class, Id, Object)).

read_class(Class, Object) :-
    findall(
        Prop-Val,
        read_property(Class, Prop, Val),
        Object
    ).

read_property(C, P, V) :-
    class_property(C, P),
    format("Input ~w (Prolog term, end with period and Enter):\n", P),
    read(V).

% -------------------- test

run :-
    read_class(person, A),
    insert(person, A).
