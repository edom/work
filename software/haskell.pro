:- module(haskell, [
    exp_con/2
    , exp_var/2
    , exp_app/3
    , exp_lam/3
    , exp_type/2
    , exp_valid/1
    , type_value/2
    , profon_haskell/2
]).
/** <module> Haskell?

Haskell terms:
    - con(T,V) constant value of type T
    - ann(T,E) expression E annotated with type T
    - var(T,Name) variable of type T
    - app(T,F,A) application with result type T
    - lam(T,A,B) abstraction with type T

To-do:
- type classes
- type synonyms
- data types
*/

/**
exp_con(?Exp, ?Con).
exp_var(?Exp, ?Var).
exp_app(?Exp, ?Fun, ?Arg).
exp_lam(?Exp, ?Var, ?Bod).

Exp constructors.
*/
exp_con(con(_,C), C).
exp_var(var(_,V), V).
exp_app(app(_,F,A), F, A).
exp_lam(lam(_,A,B), A, B).

/** exp_type(?Exp, ?Typ).

"Exp has type Typ."
*/
exp_type(con(T,_),T).
exp_type(ann(T,_),T).
exp_type(var(T,_),T).
exp_type(app(T,_,_),T).
exp_type(lam(T,_,_),T).

/** exp_valid(?Exp).

"Exp has valid type."
*/
exp_valid(con(T,V)) :- type_value(T,V).
exp_valid(ann(T,E)) :- exp_valid(E), exp_type(E,T).
exp_valid(var(_,_)).
exp_valid(app(TB, F, A)) :- exp_valid(F), exp_valid(A), exp_type(F, TA -> TB), exp_type(A, TA).
exp_valid(lam(TV -> TB, V, B)) :- exp_valid(V), exp_valid(B), exp_type(V, TV), exp_type(B, TB).

/** type_value(?Typ, ?Val).

"Typ is the type of Val."
*/
:- multifile type_value/2.
type_value('Integer',V) :- integer(V).
type_value('Integer' -> 'Integer' -> 'Integer', +).
type_value('Maybe'(_), 'Nothing').
type_value(A -> 'Maybe'(A), 'Just').
type_value(A -> 'Either'(A,_), 'Left').
type_value(B -> 'Either'(_,B), 'Right').

/** profon_haskell(?ProfonExp, ?HaskellExp).

"Profon expression ProfonExp translates to Haskell expression HaskellExp."

The syntax of ProfonExp is described in profon:exp_val/2.

Example:
```
?- profon_haskell('Just'(1), H), exp_valid(H), exp_type(H, T).
H = app('Maybe'('Integer'), con(('Integer'->'Maybe'('Integer')), 'Just'), con('Integer', 1)),
T = 'Maybe'('Integer') ;
false.
```
*/
profon_haskell(P, con('Integer',P)) :- integer(P), !.
profon_haskell(Term, Hask) :-
    functor(Term, _, _), Term =.. [F | PArgs], atom(F), !,
    profon_haskell_list(PArgs, HArgs),
    apply(con(_,F), HArgs, Hask).

profon_haskell_list([], []).
profon_haskell_list([P | Ps], [H | Hs]) :- profon_haskell(P,H), profon_haskell_list(Ps,Hs).

apply(F, [], F).
apply(F, [A | As], G) :- apply(app(_,F,A), As, G).
