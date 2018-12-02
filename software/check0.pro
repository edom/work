/** <module> a little checking

See also library(error).
*/
:- module(check0, [
    check_term/2
]).

/** check_term(?Check, ?Term)

"Run Check on Term."

Check is any of:
    - var: an unbound variable
    - nonvar: a bound variable, a partially ground term, a ground term, everything that is not an unbound variable
    - ground
    - integer
    - atom
    - string
    - A + B: check A or check B

Term may be anything.

Example:
```
check_term(var, A).
check_term(integer, 0).
check_term(var + integer, A).
check_term(var + integer, 0).
```
*/
check_term(var, A) :- var(A), !.
check_term(integer, A) :- integer(A), !.
check_term(atom, A) :- atom(A), !.
check_term(string, A) :- string(A), !.
check_term(nonvar, A) :- nonvar(A), !.
check_term(ground, A) :- ground(A), !.
check_term(Sum, A) :- nonvar(Sum), Sum = T + U, !, (check_term(T,A); check_term(U,A)).
