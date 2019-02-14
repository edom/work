:- use_module('./language_grammar.pro').
:- use_module('./language_prolog.pro').

g([
    (start :- num),
    (num :- digit | digit, num),
    (digit :- "0" ; "1" ),
    (empty :- "")
]).

main :-
    write("--- GRAMMAR ---\n"),
        g(Grammar),
        print_term(Grammar,[]),nl,
    write("--- PROLOG ---\n"),
        grammar_kb(Grammar,Rules),
        print_term(Rules,[]),nl,
    write("--- INTERPRET ---\n"),
        Input = "0110",
        string_codes(Input,Codes),
        format("Input = ~w\n", [Input]),
        format("Codes = ~w\n", [Codes]),
        kb_query(Rules, start(Codes,Rest)),
        format("Rest = ~w\n", [Rest]),
        true.
