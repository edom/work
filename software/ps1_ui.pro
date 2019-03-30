/** <module> web user interface

Types:
- A request is a list.
- A content is an abstract syntax tree.
- A method is lowercase atom. (This violates HTTP spec.)
- A html is a string containing escaped HTML markup.
*/
:- module(ps1_ui, [
    start_http/0
]).

:- use_module(library(http/http_error)).
:- use_module('./list.pro').
:- use_module('./map.pro').
:- import(file("string0.pro"),[
    strings_separator_join/3
]).
:- use_module('./website.pro', [start_http/1, content_html/2]).
:- use_module('./ps1_basic_block.pro').
:- use_module('./ps1_bit.pro').
:- use_module('./ps1_cfg.pro').
:- use_module('./ps1_decompile.pro').
:- use_module('./ps1_disassemble.pro').
:- use_module('./ps1_analysis_0.pro').

start_http :- start_http(4001).


/** formula_sentences_true(:Formula, -Sentences).

Formula has the shape Pred(A1, ..., An).

Sentences is a list.

Each element of Sentences has the shape Pred(G1, ..., Gn) where G1, ..., Gn are ground and Pred(G1, ..., Gn) are true.

Example:
```
:- module(m, [parent/2]).

parent(a,b).
parent(c,d).

?- formula_sentences(parent(_,_), [m:parent(a,b), m:parent(c,d)]).
```
*/
:- meta_predicate formula_sentences_true(0, ?).
formula_sentences_true(Formula, Sentences) :- findall(Formula, Formula, Sentences).


website:path_method_content(P,M,C) :- path_method_content(P,M,C).

path_method_content('/', get, page(
    [title('PlayStation 1 reverse-engineering assistant')],
    [
        a(href('/cfg'), 'Control flow graph')
        , h2('Routines')
        , table([
            tr([th('Address'), th('Comment')])
            , Rows
        ])
    ]
)) :- !, formula_sentences_true(routine_begin(_, _), Rows).

path_method_content('/cfg', get, page(
    [title('Control flow graph')],
    [h2('Control flow graph'), pre(Con)]
)) :- !,
    findall(C,
        (
            label_statements(Lab, Stas),
            bb_label_stas(Block, Lab, Stas),
            block_content(Block, C)
        ),
        Con).

path_method_content(Path, Method, Content) :-
    split_string(Path, "/", "", [_ | Paths]),
    paths_method_content(Paths, Method, Content).


paths_method_content(["routine", SAddr], get, page(
    [],
    [
        h2(['Routine ', routine(Addr)])
        , p(Comment)
        , h3('Decompilation')
        , pre(Decom)
        , h3('Disassembly')
        , pre(Disassembly)
    ]
)) :-
    number_string(Addr, SAddr),
    (routine_begin(Addr, Comment) -> true ; throw(error(not_routine_begin(Addr), _))),
    with_output_to(string(Disassembly), disassemble_routine(Addr)),
    with_output_to(string(Decom), decompile(Addr)).



/** block_content(+Block, -Content).

This assumes that block label is integer.
*/
block_content(Block, [span(id(SLab), [SHeader,ConH]) | IndConT]) :-
    bb_label_stas(Block, Label, Stas),
    integer_hex(Label, SLab),
    string_concat(SLab, ": ", SHeader),
    string_length(SHeader, IndAmt),
    repeat_code_string(IndAmt, 0' , IndStr),
    map(Sta, Con, sta_content(Sta,Con), Stas, [ConH|ConT]),
    map(C, [IndStr,C], ConT, IndConT), !.

repeat_code_string(Count, Code, String) :- replicate(Count, Code, List), string_codes(String, List).

sta_content(ra := A, ["ra := ", L, Com, "\n"]) :- !, addr_span(A,L), addresses_comment_content([A], Com).
sta_content(goto(A), ["goto(", L, ")", Com, "\n"]) :- !, addr_span(A,L), addresses_comment_content([A], Com).
sta_content(if(C,T,F), ["if(", SC, ",", LT, ",", LF, ")", Com, "\n"]) :- !,
    term_printed(C,SC),
    addr_span(T,LT),
    addr_span(F,LF),
    addresses_comment_content([T,F], Com).
sta_content(Sta, [Str,"\n"]) :- term_printed(Sta,Str).

addresses_comment_content(As, ["  // ", Cs]) :- addresses_comment(As, Cs), Cs \= "", !.
addresses_comment_content(_, []).

addresses_comment(As, C) :-
    map(A, C, address_comment(A,C), As, Cs),
    strings_separator_join(Cs, " ;; ", C).

address_comment(A, C) :- routine_begin(A, C), !.

addr_span(A, a(href(['#',SA]), [SA])) :- integer(A), !, integer_hex(A,SA).
addr_span(A, S) :- term_printed(A,S).

term_printed(T,P) :- term_string(T,P,[portray(true)]).


website:content_expansion(A,B) :- content_expansion(A,B).

content_expansion(hex(Int), Hex) :- !, integer_hex(Int, Hex).
content_expansion(ps1_ui:routine_begin(Addr, Comment), tr([td([routine(Addr)]), td([Comment])])) :- !.
content_expansion(routine(Addr), a(href(Href), Hex)) :- !,
    integer_hex(Addr, Hex),
    atomics_to_string(['/routine/', Hex], Href).
