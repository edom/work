% definite-clause-grammar expansion

'$my_phrase'(A,I,K) :-
    dcg_goal(A,I,K,Z),
    call(Z).

dcg_clause(A --> B, Y :- Z) :- !,
    add_args(A,[I,J],Y),
    dcg_goal(B,I,J,Z).

dcg_goal(A,I,K,Z) :- var(A), !,
    Z = '$my_phrase'(A,I,K).

dcg_goal((A,B),I,K,Z) :- !,
    Z = (X,Y),
    dcg_goal(A,I,J,X),
    dcg_goal(B,J,K,Y).

dcg_goal((A;B),I,K,Z) :- !,
    Z = (X;Y),
    dcg_goal(A,I,K,X),
    dcg_goal(B,I,K,Y).

dcg_goal(!,I,K,Z) :- !,
    Z = !,
    I = K.

dcg_goal({A},I,K,Z) :- !,
    Z = A,
    I = K.

dcg_goal(A,I,K,Z) :- is_list(A), !,
    Z = append(A,K,I).

dcg_goal(A,I,J,Z) :- string(A), !,
    string_codes(A,B),
    dcg_goal(B,I,J,Z).

dcg_goal(A,I,K,Z) :- !,
    add_args(A,[I,K],Z).

add_args(A,Args,Z) :-
    A =.. List1,
    append(List1,Args,List2),
    Z =.. List2.
