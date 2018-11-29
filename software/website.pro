:- module(website, [
    dispatch/2
    , content_html/2
]).

:- use_module(library(http/thread_httpd)).
:- use_module('./map.pro').

:- meta_predicate dispatch(3, ?).

dispatch(Path_method_content, Request) :-
    request_method_path(Request, Method, Path),
    call(Path_method_content, Path, Method, Content),
    content_html(Content, Html),
    format('Content-Type: text/html; charset=UTF-8~n'),
    format('~n'),
    write(Html).

request_method_path(_, Method, Path) :- nonvar(Method), nonvar(Path), !.
request_method_path([method(Method) | Rest], Method, Path) :- !, request_method_path(Rest, Method, Path).
request_method_path([path(Path) | Rest], Method, Path) :- !, request_method_path(Rest, Method, Path).
request_method_path([_ | Rest], Method, Path) :- !, request_method_path(Rest, Method, Path).

request_params(Request, Params) :- member(search(Params), Request), !.
request_params(_, []).

code_html(0'&, "&amp;") :- !.
code_html(0'<, "&lt;") :- !.
code_html(0'>, "&gt;") :- !.
code_html(0'", "&quot;") :- !.
code_html(C, H) :- string_codes(H, [C]).

codes_html([], "") :- !.
codes_html([A|B], H) :- !,
    code_html(A, HA),
    codes_html(B, HB),
    string_concat(HA, HB, H).

string_html(S, H) :- string_codes(S, Cs), codes_html(Cs, H).


/*
High-level content language built on top of HTML.
*/

:- multifile content_html/2.

content_html([], "") :- !.

content_html([A|B], H) :- !,
    content_html(A, HA),
    content_html(B, HB),
    string_concat(HA, HB, H).

% TODO refactor, and use library(http/html_write)
content_html(raw(S), S) :- !.
content_html(h1(C), H) :- !, content_html(C, HC), tag_content_total(h1, HC, H).
content_html(table(C), H) :- !, content_html(C, HC), tag_content_total(table, HC, H).
content_html(tr(Cs), H) :- !, content_html(Cs, TrContent), tag_content_total(tr, TrContent, H).
content_html(td(Cs), H) :- !, content_html(Cs, TdContent), tag_content_total(td, TdContent, H).
content_html(strong(A), H) :- !, content_html(A, HA), format(string(H), '<strong>~w</strong>', [HA]).

content_html(Row, H) :- row_tr(Row, Tr), !, content_html(Tr, H).

content_html(C, H) :- format(string(S), '~w', C), string_html(S, H).

row_tr(Row, tr(Tds)) :- Row =.. [row | Cols],
    map(Col, Td, col_td(Col, Td), Cols, Tds).

col_td(Col, td(Col)).

tag_content_total(Tag, Content, Total) :-
    atomics_to_string(['<', Tag, '>', Content, '</', Tag, '>'], Total).
