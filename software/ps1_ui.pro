/** <module> web user interface

Types:
- A request is a list.
- A content is an abstract syntax tree.
- A method is lowercase atom. (This violates HTTP spec.)
- A html is a string containing escaped HTML markup.
*/
:- module(ps1_ui, [
    serve/1
    , serve/0
]).

:- use_module(library(http/thread_httpd)).

serve(Port) :-
    http_server(dispatch, [port(Port)]).

serve :-
    serve(4000).

dispatch(Request) :-
    request_method_path(Request, Method, Path),
    path_method_content(Path, Method, Content),
    content_html(Content, Html),
    format('Content-Type: text/html; charset=UTF-8~n'),
    format('~n'),
    write(Html).

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

content_html([], "") :- !.
content_html([A|B], H) :- !,
    content_html(A, HA),
    content_html(B, HB),
    string_concat(HA, HB, H).
content_html(strong(A), H) :- !,
    content_html(A, HA),
    format(string(H), '<strong>~w</strong>', [HA]).
content_html(C, H) :- term_string(C, S), string_html(S, H).

request_method_path(_, Method, Path) :- nonvar(Method), nonvar(Path), !.
request_method_path([method(Method) | Rest], Method, Path) :- !, request_method_path(Rest, Method, Path).
request_method_path([path(Path) | Rest], Method, Path) :- !, request_method_path(Rest, Method, Path).
request_method_path([_ | Rest], Method, Path) :- !, request_method_path(Rest, Method, Path).

path_method_content('/', get, [strong(test), drive, ' ', get('name')]).
