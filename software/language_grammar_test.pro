:- use_module('./language_grammar.pro').
:- use_module('./language_prolog.pro').

g([
    (start :- num),
    (num :- digit ; digit, num),
    (digit :- "0" ; "1" ; "2" ; "3" ; "4" ; "5" ; "6" ; "7" ; "8" ; "9" ),
    (exp :- exp_0 ; exp_400 ; exp_500),
    (exp(0) :- num),
    (exp(0) :- "(", exp(_), ")"),
    (exp(400) :- exp(0), binop(400), exp(0) ; exp(0)),
    (exp(500) :- exp(400), binop(500), exp(400) ; exp(400)),
    (binop(400) :- "*" ; "/"),
    (binop(500) :- "+" ; "-"),
    %(what :- "" ; what, "+", "-", what, "*"; "+", ("-" ; "*", what)),
    (empty :- "")
    %(a :- b,c,d,e)
]).

main :-
    write("--- GRAMMAR ---\n"),
        g(Grammar),
        print_term(Grammar,[]),nl,
    write("--- ANALYSIS ---\n"),
        grammar_analysis(Grammar,Analysis),
        print_term(Analysis,[]),nl,
    write("--- REORDER ---\n"),
        analysis_reorder(Analysis,Reorder),
        print_term(Reorder,[]),nl,
    write("--- PROLOG 1 ---\n"),
        analysis_kb1(Reorder,Rules1),
        print_term(Rules1,[]),nl,
    write("--- PROLOG 2 ---\n"),
        grammar_kb2(Grammar,Rules2),
        print_term(Rules2,[]),nl,
    write("--- INTERPRET ---\n"),
        %Input = "(11*0110+011+11-100)+10*(11+10)",
        Input = "1+((((((((((000000001111111111111001+000000000111110))))))))))",
        string_codes(Input,Codes),
        format("Input = ~w\n", [Input]),
        format("Codes = ~w\n", [Codes]),
        kb_query(Rules1, exp(Codes)),
        %kb_query(Rules2, start(Codes,Rest)),
        format("Rest = ~w\n", [Rest]),
        true.
