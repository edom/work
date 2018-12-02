/** <module> HTML abstract syntax tree

This enhances the usability and documentation of library(http/html_write).

Actions:
    - render_ast/1

Relations:
    - ast_string/2
*/
:- module(website_html, [
    code_html/2
    , codes_html/2
    , string_html/2
    , ast_string/2
    , render_ast/1
]).

:- use_module(library(http/html_write)).

/** code_html(?Code:integer, ?Html:string) is det

"Html is the result of escaping the character whose code is Code."

At least one parameter must be bound.
*/
code_html(0'&, "&amp;") :- !.
code_html(0'<, "&lt;") :- !.
code_html(0'>, "&gt;") :- !.
code_html(0'", "&quot;") :- !.
code_html(C, H) :- string_codes(H, [C]).

/** codes_html(+Codes:list, -Html:string)

"Html is the result of escaping the string whose codes are Codes."

Codes is a list of integers.

Html is a string.
*/
codes_html([], "") :- !.
codes_html([A|B], H) :- var(H), !,
    code_html(A, HA),
    codes_html(B, HB),
    string_concat(HA, HB, H).
codes_html([A|B], H) :- string(H), !,
    string_concat(HA, HB, H),
    HA \= "",
    code_html(A, HA),
    codes_html(B, HB).

/** string_html(String:string, Html:string)

String is string.

Html is HTML-escaped String.
*/
string_html(S, H) :- string_codes(S, Cs), codes_html(Cs, H).

/** render_ast(+Ast)

Render Ast to current_output/1.

Ast follows the syntax of page//1 or page//2 in library(http/html_write):
    - Ast has the shape page(ElemList) or page(Head, Body).
    It represents the entire HTML document.
    - =Head= is an =ElemList=. It represents the contents of the =head= tag.
    - =Body= is an =ElemList=. It represents the contents of the =body= tag.
    - An =ElemList= is a list of =Elem=s.
        - An =Elem= has the shape =|Tag(AttrList, ElemList)|=.
        It represents the HTML fragment =|<Tag AttrList>ElemList</Tag>|=.
            - =AttrList= is a list of =Attr=.
                - An =Attr= has the shape =|Attr(Val)|=.
                It represents the HTML attribute =|Attr=Val|=.
            - =ElemList= is as defined above. Thus it is recursive.

Example:
```
% Using page//1.
render_ast(
    page([
        head([title('Foo')]),
        body([
            p('First paragraph.')
            , p(class(foo), 'Second paragraph with class.')
            , p([align(left), class(foo)], 'Third paragraph with many attributes.')
        ])
    ])).

% Using page//2.
render_ast(
    page(
        [title('Foo')],
        [
            p('First paragraph.')
            , p(class(foo), 'Second paragraph with class.')
            , p([align(left), class(foo)], 'Third paragraph with many attributes.')
        ]
    )).
```
*/
render_ast(Ast) :- ast_tokens(Ast, Toks), print_tokens(Toks).

/** ast_string(+Ast, -Str:string)

"Rendering Ast produces the string Str."

Ast has the shape documented in render_ast/1.
*/
ast_string(Ast, Str) :- ast_tokens(Ast, Toks), tokens_string(Toks, Str).

/** ast_tokens(+Ast, -Ren:string)

"Rendering Ast produces the token list Ren."
*/
ast_tokens(page(C), Ren) :- !, phrase(page(C), Ren).
ast_tokens(page(H, B), Ren) :- !, phrase(page(H, B), Ren).
ast_tokens(A, _) :- throw(error(invalid_page_ast(A), _)).

tokens_string(Toks, Str) :- with_output_to(string(Str), print_tokens(Toks)).

print_tokens(Toks) :- print_html(Toks).
