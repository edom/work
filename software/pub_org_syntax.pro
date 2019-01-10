/** <module> Org Mode syntax

Org Mode syntax is too complex.
We only parse a subset of it.
I only parse the subset that I use.

https://orgmode.org/worg/dev/org-syntax.html

*/
:- module(pub_org_syntax, [
    document/5
]).
:- use_module('./dcg_stateful.pro').

par_ast([s(S)|T]) --> span(S), !, par_ast(T).
par_ast([fi(Na,Te)|T]) --> [fi(Na,Te)], !, par_ast(T).
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
        paragraph([A|B]) => par_thing(A), paragraph(B).
        paragraph([c(10)]) => [10], \+nl.
        paragraph([c(10)]) => [10], \+star.
        paragraph([c(C)|T]) => [C], paragraph(T).
        paragraph([]) => [].
            % [fn:A:B] difference: org mode disallows space in A; we allow.
            par_thing(fi('',Text)) => "[fn::", fnelem(Text), "]".
            par_thing(fi(Name,Text)) => "[fn:", fnelem_atom(Name), ":", fnelem(Text), "]".
            par_thing(fi(Name,nil)) => "[fn:", fnelem_atom(Name), "]".
            par_thing(texi(F)) => texinline(F).
            par_thing(latex_env(E,A,B)) => latex_env(E,A,B).
                fnelem([]) => [].
                fnelem([H|T]) => [H], {H \= 0':}, fnelem(T).
                fnelem_atom(A) => fnelem(C), {atom_codes(A,C)}.
                latex_env(E,A,B) => latex_begin(E,A), latex(B), latex_end(E).
                    latex_begin(E,A) => "\\begin{", reluctant_atom(E), "}", latex_optarg(A).
                        latex_optarg(A) => "[", latex(A), "]".
                        latex_optarg(nil) => [].
                    latex_end(E) => "\\end{", reluctant_atom(E), "}".
                    latex([]) => [].
                    latex([H|T]) => latex_thing(H), latex(T).
                    latex_thing(latex_env(E,A,B)) => latex_env(E,A,B).
                    latex_thing(H) => [H].
                    reluctant([]) => [].
                    reluctant([H|T]) => [H], reluctant(T).
                    reluctant_atom(A) => reluctant(C), {atom_codes(A,C)}.
                    reluctant_string(S) => reluctant(C), {string_codes(S,C)}.
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
