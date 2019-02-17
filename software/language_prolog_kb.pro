:- module(language_prolog_kb,[
    kb_rule/2,
    kb_head_body/3
]).

/** kb_rule(+Rules,?Rule)

Unify Rule with each rule in Rules.
*/
kb_rule([A|_],R) :- copyterm0(A,R).
kb_rule([_|A],R) :- kb_rule(A,R).

    % duplicate_term/2 is slow.
    copyterm0((H0:-B0),(H1:-B1)) :- !, unifiable(H0,H1,_), duplicate_term((H0:-B0),(H1:-B1)).
    copyterm0(A,B) :- unifiable(A,B,_), duplicate_term(A,B).

/** kb_head_body(+List, ?Head, ?Body)

Match (Head :- Body) in List; or match Head in List and set Body = true.

This is similar to clause/2, but this looks only in the List instead of all loaded definitions.

This cannot be done by member/2 because this requires copy_term/2.
*/
kb_head_body([C|_],H,B) :- copyterm0(C,(H:-B)).
kb_head_body([C|_],H,true) :- copyterm0(C,H).
kb_head_body([_|R],H,B) :- kb_head_body(R,H,B).
