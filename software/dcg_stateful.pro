:- module(dcg_stateful, [
    op(1200,xfx,'=>'),
    phrase_stateful/3,
    phrase0/3,
    test/0
]).

:- op(1200, xfx, '=>').

% It is easier to write an expansion than to write a meta-interpreter.

/** state_arg(Index,Name,Init)
*/
state_arg(1,line,1).
state_arg(2,column,1).

state_argcount(2).

% Keep init and arg consistent.
state_init(S) :-
    state_argcount(N),
    functor(S,s,N),
    foreach(state_arg(Index,_Name,Init), arg(Index,S,Init)).


state_command([]) :- !.
state_command([H|T]) :- !, state_command(H), state_command(T).
state_command((A,B)) :- !, state_command(A), state_command(B).
state_command(Val = Exp) :- !, state_exp_val(Exp,Val).
state_command(A) :- throw(error(state_command(A), _)).


state_exp_val(get(Name,S), Val) :- !,
    state_arg(A,Name,_),
    arg(A,S,Val).

state_exp_val(set(Name,New,S), S1) :- !,
    state_exp_val(New,ENew),
    state_arg(A,Name,_),
    copy_term(S,S1),
    nb_setarg(A,S1,ENew).

state_exp_val(A+B, C) :- !, state_exp_val(A,EA), state_exp_val(B,EB), C is EA+EB.
state_exp_val(A,A) :- number(A), !.
state_exp_val(A,A) :- string(A), !.
state_exp_val(A,_) :- throw(error(state_exp(A))).

state_codes_next(S, [], S).
state_codes_next(A, [H|T], C) :-
    (H = 10
    ->  state_command([B0=set(line, get(line,A)+1, A), B=set(column,1,B0)])
    ;   state_command(B=set(column, get(column,A)+1, A))
    ),
    state_codes_next(B,T,C).

phrase_stateful(Start, Input0, Input1) :-
    state_init(State0),
    phrase_stateful(Start, State0, _, Input0, Input1).

phrase_stateful(Start, State0, State1, Input0, Input1) :-
    call(Start, State0, State1, Input0, Input1).

expand_head(H, HX, S0, S1, I0, I1) :-
    H =.. Hs,
    append(Hs, [S0, S1, I0, I1], HXs),
    HX =.. HXs.

expand_body(S0, S9, I0, I9, B, BX) :-
    var(B)      ->  BX=(expand_body(S0,S9,I0,I9,B,BE), BE)
;   B=!         ->  BX=(S0=S9, I0=I9, B)
;   B={BA}      ->  BX=(S0=S9, I0=I9, BA)
;   B=(\+N)     ->  BX=((\+BN), S0=S9, I0=I9), expand_body(S0,S9,I0,I9,N,BN)
;   B=(BA,BB)   ->  BX=(BAX,BBX), expand_body(S0, S, I0, I, BA, BAX), expand_body(S, S9, I, I9, BB, BBX)
;   B=(BA;BB)   ->  BX=(BAX;BBX), expand_body(S0, S9, I0, I9, BA, BAX), expand_body(S0, S9, I0, I9, BB, BBX)
;   string(B)   ->  string_codes(B,Cs), expand_body(S0,S9,I0,I9,Cs,BX)
;   is_listy(B) ->  BX=(append(B,I9,I0), state_codes_next(S0,B,S9))
;   expand_head(B, BX, S0, S9, I0, I9).

is_listy([]).
is_listy([_|_]).

expand_rule((H => B), (HX :- BX)) :-
    expand_head(H, HX, S0, S9, I0, I9),
    expand_body(S0, S9, I0, I9, B, BX).

term_expansion(A,B) :- expand_rule(A,B).

/** phrase0(Start, Input, Rest)

Grammar is a list of rules.
*/
phrase0(Start, Input, Rest) :-
    state_init(State0),
    Start =.. LStart,
    append(LStart, [Input, Rest], LStartP),
    StartP =.. LStartP,
    phrase1(State0, _, StartP, Input, Rest).

phrase1(S, S, true, I, I) :- !.
phrase1(S0, S1, phrase(A,B,C), B, C) :- !, phrase1(S0,S1,A,B,C). % ???
phrase1(S, S, current_column(C), I, I) :- !, state_command(C=get(column,S)).

phrase1(S0, S2, (A,B), Input, Rest) :- !,
    phrase1(S0,S1,A,Input,J),
    phrase1(S1,S2,B,J,Rest).

phrase1(S0, S1, (A;B), Input, Rest) :- !,
    (   phrase1(S0,S1,A,Input,Rest)
    ;   phrase1(S0,S1,B,Input,Rest)
    ).

phrase1(S0, S1, Head, Input, Rest) :-
    functor(Head, Name, Arity),
    clause(Head, Body),
    Body,
    !.

test :-
    %nospy(expand_body/6),
    %Rest=[],
    %string_codes("012210", Codes), phrase_stateful(number(N), Codes, Rest), print(N-Rest),
    string_codes(
"\\( x = 2 \\)",
        Codes), state_init(S), phrase_stateful(document(D), S, SS, Codes, Rest), print(D-Rest-SS),
    %string_codes("** Foo", Codes), phrase0(headline(S,T), Codes, Rest), print(S-T-Rest),
    nl.

eof(S,S,[],[]).
lookahead(A,S,S,I,I) :- append(A,_,I).
current_column(N,S,S,I,I) :- state_command(N=get(column,S)).

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
