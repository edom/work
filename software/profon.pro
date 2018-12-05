:- module(profon, [
    exp_val/2
    , exp_expansion/2
]).
/** <module> PROgrammation en FONctions, make functional Prolog somewhat more convenient

This module is not too ambitious; it merely tries to make functional programming in Prolog somewhat more convenient.

You may also be interested in these:
    - Other languages:
        - Haskell
        - Mercury
        - Curry
        - Goedel
    - Other embedded languages:
        - Racklog
        - Clojure core.logic
        - Scheme miniKanren
        - microKanren
    - 2018-12-06
        - unmaintained: a Prolog interpreter written in Haskell http://hackage.haskell.org/package/prolog
        - Somewhat ambitious: https://github.com/jarble/functional-prolog/blob/master/functional_prolog.pl
*/

/** exp_val(+Exp, -Val).

"Exp evaluates to Val."

Evaluation rules (English commentary of the Prolog source code):
    - If Exp is a current_arithmetic_function/1 (anything that is a valid right-hand side of is/2),
    then Exp evaluates according to is/2.
        - See "Arithmetic functions" in [SWI-Prolog arithmetics documentation](http://www.swi-prolog.org/pldoc/man?section=arith).
        See also is/2 and current_arithmetic_function/1.
    - If Exp = F(A1, ..., An) where F is a current_predicate/2 with arity n+1,
    then F/n+1 is called, and Exp evaluates to whatever value bound to the last parameter of F/n+1.
    Note that the arity of the relation is one plus the arity of the function.
    Thus, every function is a relation, as in mathematics.
        - See current_predicate/2.
        Note that exp_val/2 does not autoload predicates.
    - If exp_expansion/2 cannot expand Exp, then Val is Exp itself.
    - Otherwise, apply exp_expansion/2 repeatedly to Exp until failure.

To-do?
- lambda abstraction?
- currying?
- higher-order function?
*/
exp_val(E, V) :-
    functor(E, H, FunArity),
    RelArity is FunArity + 1,
    modpred_arity_indicator(H, RelArity, Ind),
    current_predicate(Ind), !,
    call(E, V).

exp_val(E, V) :-
    current_arithmetic_function(E), !,
    E =.. [F | Args],
    exp_val_list(Args, Vals),
    H =.. [F | Vals],
    V is H.

exp_val(E, V) :- exp_expansion(E, EE) -> exp_val(EE, V) ; E = V.

/** exp_expansion(?In, ?Out).

"In expands to Out."

This is a hook for user-defined expansions.

Make sure that clause order does not affect expansion.
*/
:- multifile exp_expansion/2.

modpred_arity_indicator(Mod:Pred, Arity, Mod:Pred/Arity) :- !.
modpred_arity_indicator(Pred, Arity, Pred/Arity) :- !.

exp_val_list([], []).
exp_val_list([E | Es], [V | Vs]) :- exp_val(E,V), exp_val_list(Es,Vs).
