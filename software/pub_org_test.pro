:- module(pub_org_test, [
    test/0,
    test_footnote/0,
    print_hlist/1,
    tree_hlist/2,
    hlist_str//2
]).
:- use_module('./dcg_stateful.pro').
:- use_module('./pub_org_process.pro').
:- use_module('./pub_org_syntax.pro').

test :-
    string_codes(
"
\\begin{theorem}[foo]
\\begin{proof}
Done.
\\end{proof}
\\end{theorem}
",
        Codes), state_init(S), phrase_stateful(document(D), S, Codes, SS, Rest), print(D-Rest-SS), nl,
        !,
    nl, !.

test_footnote :-
    string_codes("[fn:a][fn:a:b][fn::b][fn:a]", Codes),
    phrase_stateful(document(D), Codes, []), !,
    print(D), nl,
    process_footnotes([], D, F1, D1),
    print(F1-D1), nl.

tree_hlist([H|T], [HL|TL]) :- !, tree_hlist(H,HL), tree_hlist(T,TL).
tree_hlist(H, [F|LL]) :- functor(H,_,Arity), Arity > 0, !, H =.. [F|T], tree_hlist(T, LL).
tree_hlist(H, [H]).

print_hlist(T) :- phrase(hlist_str(0,T),C,[]), string_codes(S,C), write(S), nl.

hlist_str(_,[]) --> !.
hlist_str(Ind,[H|T]) --> !, {I2 is Ind+2}, hlist_str(I2,H), hlist_str(Ind,T).
hlist_str(Ind,T) --> {term_string(T,S), string_codes(S,C)}, indent(Ind), C, "\n".

indent(N) --> {N =< 0, !}, "".
indent(N) --> {N > 0, !, N1 is N-1}, " ", indent(N1).
