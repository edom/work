:- use_module('./language_prolog.pro').

% This does not terminate if the graph is cyclic.
main :-
    Rules = [
        e(a,b),
        e(b,c),
        (reach(A,B) :- e(A,B) ; e(A,C), reach(C,B)),
        true
    ],
    Goal = (reach(_,_)),
    kb_query(Rules,Goal),
    print(Goal),nl,
    true.
