:- module(dcg_stateful, [
    % public
    phrase_stateful/3,
    op(1200,xfx,'=>'),
    term_expansion/2, % rewrite =>
    % advanced usage
    phrase_stateful/5,
    state_init/1,
    % combinators
    eof/4,
    lookahead/5,
    current_column/5
]).
/** <module>
The idea is to add two more parameters to DCG rules: state-in and state-next.

If DCG ads two parameters (input, rest), then stateful DCG adds four parameters: (state-in, input, state-next, rest)

The predicate phrase_stateful/3 is analogous to the predicate phrase/3.
*/

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

:- meta_predicate(phrase_stateful(:,?,?)).
:- meta_predicate(phrase_stateful(:,?,?,?,?)).

phrase_stateful(Start, Input0, Input1) :-
    state_init(State0),
    phrase_stateful(Start, State0, Input0, _, Input1).

phrase_stateful(Start, State0, Input0, State1, Input1) :-
    call(Start, State0, Input0, State1, Input1).

expand_head(H, HX, S0, I0, S1, I1) :-
    H =.. Hs,
    append(Hs, [S0, I0, S1, I1], HXs),
    HX =.. HXs.

expand_body(S0, I0, S9, I9, B, BX) :-
    var(B)      ->  BX=(expand_body(S0,I0,S9,I9,B,BE), BE)
;   B=!         ->  BX=(S0=S9, I0=I9, B)
;   B={BA}      ->  BX=(S0=S9, I0=I9, BA)
;   B=(\+N)     ->  BX=((\+BN), S0=S9, I0=I9), expand_body(S0,I0,S9,I9,N,BN)
% Why does this hang make/0 in prolog_clause?
;   B=(BA,BB)   ->  BX=(BAX,BBX), expand_body(S0, I0, S, I, BA, BAX), expand_body(S, I, S9, I9, BB, BBX)
;   B=(BA;BB)   ->  BX=(BAX;BBX), expand_body(S0, I0, S9, I9, BA, BAX), expand_body(S0, I0, S9, I9, BB, BBX)
;   string(B)   ->  string_codes(B,Cs), expand_body(S0,I0,S9,I9,Cs,BX)
;   is_listy(B) ->  BX=(append(B,I9,I0), dcg_stateful:state_codes_next(S0,B,S9))
;   expand_head(B, BX, S0, I0, S9, I9).

is_listy([]).
is_listy([_|_]).

expand_rule((H => B), (HX :- BX)) :-
    expand_head(H, HX, S0, I0, S9, I9),
    expand_body(S0, I0, S9, I9, B, BX).

term_expansion((H=>B), (HX:-BX)) :- expand_rule((H=>B), (HX:-BX)).

eof(S,S,[],[]).

lookahead(A,S,S,I,I) :- append(A,_,I).

current_column(N,S,S,I,I) :- state_command(N=get(column,S)).
