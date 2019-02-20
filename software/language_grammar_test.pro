:- use_module('./language_grammar.pro').
:- use_module('./language_prolog.pro').

g([
    (start :- exp),
    (num :- digit ; digit, num),
    (digit :- "0" ; "1" ; "2" ; "3" ; "4" ; "5" ; "6" ; "7" ; "8" ; "9" ),
    (exp :- num ; exp_paren ; exp_mul ; exp_plus ; exp_minus),
    (exp_paren :- "(", exp, ")"),
    (exp_mul :- exp, "*", exp),
    (exp_plus :- exp, "+", exp),
    (exp_minus :- exp, "-", exp),
    /*
    (exp :- exp(0) ; exp(400) ; exp(500)),
    (exp(0) :- num),
    (exp(0) :- "(", exp(_), ")"),
    (exp(400) :- exp(0), binop(400), exp(0) ; exp(0)),
    (exp(500) :- exp(400), binop(500), exp(400) ; exp(400)),
    (binop(400) :- "*" ; "/"),
    (binop(500) :- "+" ; "-"),
    */
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
        Directives = [
            (:-memoize(num/1)),
            (:-memoize(exp/1)),
            (:-memoize(exp_paren/1)),
            (:-memoize(exp_mul/1)),
            (:-memoize(exp_plus/1)),
            (:-memoize(exp_minus/1))
        ],
        append(Directives,Rules1,Rules11),
        print_term(Rules11,[]),nl,
    write("--- PROLOG 2 ---\n"),
        grammar_kb2(Grammar,Rules2),
        print_term(Rules2,[]),nl,
    write("--- INTERPRET ---\n"),
        Input = "(112342342342341212+1414141-222+3131222*5151-4441+3123123+1112-((51212414+121487187492873)*4141)*0110+011+11-100)+10*(11+10)",
        %Input = "1+((((((((((0+1))))))))))",
        %Input = "(1)",
        %Input = "(1+2+3+4+5)+(2)+(3)",
        string_codes(Input,Codes),
        format("Input = ~w\n", [Input]),
        format("Codes = ~w\n", [Codes]),
        kb_query(Rules11, exp(Codes)),
        %kb_query(Rules2, start(Codes,Rest)),
        format("Rest = ~w\n", [Rest]),
        true.

main1 :-
    g(G),
    grammar_info(G,I),
    print_term(I,[]),nl,
    grammar_left_recursions(G,L),
    print(L),nl.
