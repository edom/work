/** <module> Org Mode syntax

Org Mode syntax is too complex.
We only parse a subset of it.
I only parse the subset that I use.

https://orgmode.org/worg/dev/org-syntax.html

*/
:- module(org_syntax, [
    test/0,
    print_tree/1,
    tree_str//2
]).
:- use_module('./dcg_stateful.pro').

test :-
    string_codes(
"\\( x = 2 \\)",
        Codes), state_init(S), phrase_stateful(document(D), S, Codes, SS, Rest), print(D-Rest-SS),
    print_tree(D),
    nl.

print_tree(T) :- phrase(tree_str(0,T),C,[]), string_codes(S,C), write(S), nl.

%tree_str(Ind,'[|]') --> !, indent(Ind), "[|]\n".
%tree_str(Ind,[]) --> !, indent(Ind), "[]\n".
tree_str(Ind,A) --> {atomic(A), !, term_string(A,S), string_codes(S,C)}, indent(Ind), C, "\n".
%tree_str(Ind,A) --> {A =.. [H|T], I2 is Ind+2}, tree_str(Ind,H), tree_str(I2,T).

indent(N) --> {N =< 0, !}, "".
indent(N) --> {N > 0, !, N1 is N-1}, " ", indent(N1).

par_ast([s(S)|T]) --> span(C), !, {string_codes(S,C)}, par_ast(T).
par_ast([fn(S)|T]) --> [fn(C)], !, {string_codes(S,C)}, par_ast(T). % inline footnote
par_ast([fr(S)|T]) --> [fr(C)], !, {string_codes(S,C)}, par_ast(T). % footnote reference
par_ast([texi(S)|T]) --> [texi(C)], !, {string_codes(S,C)}, par_ast(T).
par_ast([U|T]) --> [U], !, par_ast(T).
par_ast([]) --> [].
    span([H|T]) --> [c(H)], span(T).
    span([H]) --> [c(H)].

document([T|D]) => thing(T), document(D).
document([]) => [].
    thing(headline(S,T)) => headline(S,C), {string_codes(T,C)}.
    thing(nl) => nl.
    thing(paragraph(A)) => paragraph(C), {C \= [], par_ast(A,C,[])}.
        headline(S,T) => current_column(1), stars(S), title(T).
            stars(N1) => star, stars(N), {N1 is N+1}.
            stars(1) => star.
            star => "*".
            title([C|T]) => [C], {C \= 10}, title(T).
            title([]) => "".
        nl => "\n".
        paragraph([fn(F)|T]) => fninline(F), paragraph(T).
        paragraph([fr(F)|T]) => fnref(F), paragraph(T).
        paragraph([texi(F)|T]) => texinline(F), paragraph(T).
        paragraph([c(10)]) => [10], \+nl.
        paragraph([c(10)]) => [10], \+star.
        paragraph([c(C)|T]) => [C], paragraph(T).
        paragraph([]) => [].
            %par_span_end => fnbegin.
            %par_span_end => texibegin.
            fnbegin => "[fn:".
            fninline(F) => fnbegin, ":", fntext(F), "]".
                fntext([H|T]) => [H], {H \= 0']}, fntext(T).
                fntext([]) => [].
            fnref(F) => fnbegin, \+":", fntext(F), "]".
            texibegin => "\\(".
            texiend => "\\)".
            texinline(F) => texibegin, texcontent(F).
            texcontent([H|T]) => \+texiend, [H], texcontent(T).
            texcontent([]) => texiend.
        /*
            latex_command(Name,Args) =>
                "\\", latex_command_name(Name),
                {latex_command_arity(Name, Arity) -> true ; throw(error(latex_command(Name), _))},
                latex_command_arguments(Arity, Args).
                latex_command_name(Name) => ltx_cmd_nam(N), {string_codes(Name,N)}.
                    ltx_cmd_nam([H|T]) => [H], {code_type(H,csym)}, ltx_cmd_nam(T).
                    ltx_cmd_nam([]) => [].
                ltx_cmd_args(N, [H|T]) => {N > 0}, ltx_tok(H), {N1 is N-1}, ltx_cmd_args(N1,T).
                ltx_cmd_args(N, []) => {N =< 0}.
            %ltx_tok(lg(T)) => ltx_grp(T).
            %ltx_tok(lc(T)) => ltx_cmd(T).
            latex_env_begin(Name) => "\\begin{", latex_env_name(Name), "}".
            latex_env_name([H|T]) => \+"}", [H], latex_env_name(T).
            latex_env_name([]) => [].
        */

/** latex_command_arity(?Name, ?Arity).

Arity is the number of mandatory arguments.
*/
latex_command_arity(newcommand, 2).
latex_command_arity(emph, 1).
latex_command_arity(begin, 1).
latex_command_arity(end, 1).

digit(D) => {member(D,[0,1,2,3,4,5,6,7,8,9]), number_string(D,S)}, S.
number([H|T]) => digit(H), number(T).
number([]) => "".
