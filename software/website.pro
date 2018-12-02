/** <module> opinionated website framework

## Usage

This module is very hardcoded, opinionated, and inflexible.

Simplifying assumptions:
    - One Prolog process runs at most one HTTP server.

Required:
    - Extend path_method_content/3.
    - Call start_http/1.

Optional:
    - Extend content_expansion/2.

### Stack traces

To make the server shows the stack trace of errors, add:
```
:- use_module(library(http/http_error)).
```

Don't do that in production.

## TODO?

Features:
    - Uniform table rendering
        - Mapping predicate to display table: predicate_table/2.
*/
:- module(website, [
    start_http/1
    , path_method_content/3
    , content_html/2
    , content_expansion/2
    , is_html_tag_name/1
    , predicate_table/2
]).

:- use_module(library(http/thread_httpd)).
:- use_module('./map.pro').
:- use_module('./website_html.pro').

/** start_http(+Port:integer)

"Start listening at the TCP port Port."
*/
start_http(Port) :- http_server(dispatch, [port(Port)]).


/** path_method_content(?Path, ?Method, ?Content)

"The response to the request with path Path and method Method is Content."

Path is an atom beginning with slash, such as `'/foo'`.

Method is a lowercase atom, such as `get` or `post`.

Content is a high-level-content-language abstract-syntax-tree defined in content_html/2.
*/
:- multifile path_method_content/3.


/** dispatch(Request)

This is the opinionated part.
*/
dispatch(Request) :-
    request_method_path(Request, Method, Path),
    path_method_content_(Path, Method, Content),
    content_html(Content, Html),
    format('Content-Type: text/html; charset=UTF-8~n'),
    format('~n'),
    render_ast(Html).

path_method_content_(P,M,C) :- path_method_content(P,M,C), !.
path_method_content_(P,M,_) :- throw(error(not_found(M,P), _)).

request_method_path(_, Method, Path) :- nonvar(Method), nonvar(Path), !.
request_method_path([method(Method) | Rest], Method, Path) :- !, request_method_path(Rest, Method, Path).
request_method_path([path(Path) | Rest], Method, Path) :- !, request_method_path(Rest, Method, Path).
request_method_path([_ | Rest], Method, Path) :- !, request_method_path(Rest, Method, Path).

request_params(Request, Params) :- member(search(Params), Request), !.
request_params(_, []).


/** content_html(+Content, -Html)

"The high-level-content-language AST Content translates to the HTML AST Html."

Content is an abstract syntax tree of high-level content language.
    - This predicate flattens nested lists in Content.
    - Every HTML attribute value that is a list, is flattened into string.

Html is HTML AST as defined in render_ast/1.

You can extend the language by defining content_expansion/2.
*/
content_html([], []) :- !.
content_html([A|B], H) :- !, content_html(A,HA), content_html(B,HB), maybe_append(HA,HB,H).
content_html(page(A), page(H)) :- !, content_html(A,H).
content_html(page(H,B), page(HH,HB)) :- !, content_html(H,HH), content_html(B,HB).
content_html(raw(C), \[C]) :- atom(C), !.
content_html(raw(C), \[C]) :- string(C), !.
content_html(raw(C), _) :- !, throw(error(must_be_atom_or_string(C), _)).
%content_html(RowHeader, H) :- row_header_tr(RowHeader, H), !.
%content_html(Row, H) :- row_tr(Row, H), !.
content_html(A, B) :- html_html(A, B), !.
content_html(A, A) :- atom(A), !.
content_html(A, A) :- string(A), !.
content_html(A, A) :- integer(A), !.
content_html(A, H) :- content_expansion(A, B), !, content_html(B, H).
content_html(A, _) :- throw(error(invalid_content(A), _)).

/** content_expansion(+Pattern, -Expansion)

"content_html/2 should recursively expand every matching occurrence of Pattern to the corresponding Expansion."

Important notes:
    - Every Pattern must match zero or one clause but not more.
    - Clause order must not affect Expansion.
    - Do not redefine built-in HTML tags defined by is_html_tag_name/1.
    Note that the set of HTML tag names may change.
    - Do not call content_html/2 from content_expansion/2.
    You only need to define _one step_ of the expansion.

Example:
```
website:content_expansion(A,B) :- content_expansion(A,B).

content_expansion(strongem(A), strong(em(A))) :- !.
content_expansion(dup(A), [A,A]) :- !.
```
*/
:- multifile content_expansion/2.


/** maybe_append(A, B, C)

Like append/3, but with implicit conversion of non-listlike to singleton list.

The meaning of "listlike" is defined in is_listlike/1.
*/
maybe_append(A, B, C) :-
    thing_list(A, LA),
    thing_list(B, LB),
    append(LA, LB, C).


/** thing_list(?Thing, ?List)

"Wrap Thing in a list if Thing is not already listlike."

Thing should not be an improper list.

The meaning of "listlike" is defined in is_listlike/1.
*/
thing_list(A, A) :- is_listlike(A), !.
thing_list(A, [A]).


/** is_listlike(?Thing)

"Thing is either the empty list or a cons."
*/
is_listlike([]).
is_listlike([_|_]).


html_html(Term, Html) :-
    term_tag_attrs_children(Term, Tag, Attrs, Children),
    is_html_tag_name(Tag), !,
    map(A, NA, attr_normalized(A,NA), Attrs, NAttrs),
    print_message(debug(html), vals(Attrs, NAttrs)),
    content_html(Children, HtmlChildren),
    term_tag_attrs_children(Html, Tag, NAttrs, HtmlChildren).

attr_normalized(A, N) :-
    A =.. [K|V],
    attval_normalized(V, NV),
    N =.. [K,NV].

attval_normalized([], "") :- !.
attval_normalized([H|T], S) :- !, attval_normalized(H,NH), attval_normalized(T,NT), string_concat(NH,NT,S).
attval_normalized(A,B) :- atom(A), !, atom_string(A,B).
attval_normalized(A,A) :- string(A), !.
attval_normalized(A,_) :- throw(error(cannot_normalize_attr_value(A), _)).

/** term_tag_attrs_children(Term, Tag, Attrs, Children)

Normalize a HTML AST node into Tag(Attrs, Children).
*/
term_tag_attrs_children(Term, Tag, Attrs, Ch) :- var(Term), !, thing_list(Ch, Children), Term =.. [Tag, Attrs, Children].
term_tag_attrs_children(Term, Tag, Attrs, Children) :- Term =.. [Tag, At, Ch], !, thing_list(At, Attrs), thing_list(Ch, Children).
term_tag_attrs_children(Term, Tag, [], Children) :- Term =.. [Tag, Ch], !, thing_list(Ch, Children).


/** is_html_tag_name(?Tag)

"Tag is a HTML tag name."

Tag must be lowercase.
*/
is_html_tag_name(Tag) :- member(Tag, [
    a, address, article, aside, body, caption, cite, code, div, em, h1, h2, h3, h4, h5, h6
    , head, kbd, link, meta, nav, p, pre, script, section, span, strong, style, table, tbody, tfoot, thead, title, th, tr, td
]).


row_header_tr(Row, tr(Ths)) :- Row =.. [row_header | Cols],
    map(Col, Th, col_th(Col, Th), Cols, Ths).

row_tr(Row, tr(Tds)) :- Row =.. [row | Cols],
    map(Col, Td, col_td(Col, Td), Cols, Tds).

col_th(th(A), th(H)) :- !, content_html(A, H).
col_th(Col, th(H)) :- content_html(Col, H).

col_td(td(A), td(H)) :- !, content_html(A, H).
col_td(Col, td(H)) :- content_html(Col, H).

/** predicate_table(?Pred, ?Table)

"Predicate Pred can be displayed as table Table."

Pred is Predicate/Arity such as =predicate_table/2=.

Table is a list:
    - columns(Cols)

Example:
```
predicate_table(person/2, [
    columns(['Name', 'Address'])
]).

type_value_html(T, V, H)
```
*/
:- multifile predicate_table/2.
