:- discontiguous dcg_clause/2.

term_expansion(A-->B, dcg_clause(A,B)).

digit --> "0".
digit --> "1".

number --> digit.
number --> digit, number.

interpret((A,B),Cst,L0,L9) :- !,
    Cst = (CA,CB),
    interpret(A,CA,L0,L1),
    interpret(B,CB,L1,L9).

interpret(H,Cst,L0,L9) :- string(H), !,
    string_codes(H,C),
    interpret(C,Cst,L0,L9).

interpret(H,Cst,L0,L9) :- dcg_clause(H,B),
    Cst = H-CB,
    interpret(B,CB,L0,L9).

interpret([],Cst,L0,L9) :- !, Cst = [], L0 = L9.

interpret([H|T],Cst,[H|L0],L9) :-
    Cst = [H|CstT],
    interpret(T,CstT,L0,L9).

test :-
    string_codes("01101",In),
    interpret(number,Cst,In,[]),
    print_term(Cst,[]),nl,
    meaning(Cst,Ast),
    print_term(Ast,[]),nl.

meaning(digit-[0'0],0).
meaning(digit-[0'1],1).
/*
The meaning of a number is more convenient if number is left-recursive,
because the digits are already reversed by the parser.

meaning(number-(A,B),M) :-
    meaning(A,MA),
    meaning(B,MB),
    M is 10*MA+MB.
*/

