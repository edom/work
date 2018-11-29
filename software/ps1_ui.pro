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
:- use_module('./map.pro').
:- use_module('./ps1_disassemble.pro').
:- use_module('./ps1_analysis_0.pro').
:- use_module('./website.pro').

serve(Port) :- http_server(dispatch(path_method_content), [port(Port)]).

serve :- serve(4001).


path_method_content('/', get, Content) :-
    Content = [
        h1('Routines')
        , table(Rows)
    ],
    findall(row(memory_address(Addr), Comment), routine_begin(Addr, Comment), Rows).

path_method_content(Path, Method, Content) :-
    split_string(Path, "/", "", [_ | Paths]),
    paths_method_content(Paths, Method, Content).


paths_method_content(["routine", SAddr], get, raw(Content)) :-
    number_string(Addr, SAddr),
    routine_begin(Addr, Comment),
    with_output_to(string(S), disassemble_routine(Addr)),
    atomics_to_string([Comment, '<pre>', S, '</pre>'], Content).

paths_method_content(Paths, Method, _) :- throw(paths_method(Paths, Method)).


website:content_html(memory_address(A), S) :- !, format(atom(S), '0x~16r', [A]).
