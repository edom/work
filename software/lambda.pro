% Lambda calculus.

:- module(lambda, [
    reduce/2
]).

/*
A lambda term is any of these:

    con(A)
    var(A) where A is an atom that represents the variable name
    lam(V,B) where V is an atom that represents the variable (note that V is an atom, not a compound var(C))
    app(A,B)
*/

% Beta-reduction, in normal-order, head-normal form.

reduce(con(A), con(A)).

reduce(var(A), var(A)).

% Isn't this case too aggressive?
reduce(lam(V,B0), lam(V,B1)) :- reduce(B0, B1).

reduce(app(lam(V,B),P), Res1) :- true
    , substitute(V, P, B, Res0)
    , reduce(Res0, Res1)
    .

reduce(app(A,B), app(A0,B0)) :- true
    , A \= lam(_,_)
    , reduce(A,A0)
    , reduce(B,B0)
    .

% Replace every free occurrence of Var in Exp with Sub, producing Res.

substitute(_, _, con(A), con(A)).
substitute(Var, Sub, var(Var), Sub).
substitute(Var, _, lam(Var,Bod), lam(Var,Bod)).
substitute(Var, Sub, lam(Var0,Bod0), lam(Var0,Bod1)) :- true
    , Var \= Var0
    , substitute(Var, Sub, Bod0, Bod1)
    .
substitute(Var, Sub, app(A,B), app(A0,B0)) :- true
    , substitute(Var, Sub, A, A0)
    , substitute(Var, Sub, B, B0)
    .
