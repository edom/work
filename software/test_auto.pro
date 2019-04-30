:- use_module(library(pprint),[
    print_term/2
]).

% -------------------- what is tested

:- import(file("html_cphe.pro"),[
    cphe_ast/2
    , cphe_string/2
]).
/*
%   Broken due to different loading mechanism (load.pro vs load0.pro).

:- import(file("sketch_application.pro"),[
    start_http_server/0
    , check_web_app/0
]).
:- import(file("enterprise/library/sql.pro"),[
    sql_ddl_create_table/2
]).
*/

% -------------------- the tests

test(A) :- clause(run(A), _).

run(html_cphe_attribute) :-
    Exp = p(a=b, "T", c=d, b("U", "V")),
    cphe_ast(Exp, Ast),
    pretty(Exp), nl,
    pretty(Ast), nl,
    actual_expected(
        Ast,
        elem(p,[attr(a,b),attr(c,d)],[text("T"),elem(b,[],[text("U"),text("V")])])
    ).

run(html_cphe_string) :-
    Exp = a(href="foo.png", "The image"),
    cphe_string(Exp, Str),
    pretty(Exp), nl,
    write(Str), nl.

run(html_cphe_string_empty_tag) :-
    Exp = img(src="foo.png", alt="bar"),
    cphe_string(Exp, Str),
    pretty(Exp), nl,
    write(Str), nl.

run(html_cphe_document) :-
    Exp = ['!doctype'(html), html(head(title("Title")), body(p("Paragraph")))],
    cphe_string(Exp, Str),
    pretty(Exp), nl,
    write(Str), nl.

run(check_web_app) :-
    check_web_app.

run(sql_ddl_create_table) :-
    forall(sql_ddl_create_table(_, Sql), (
        write(Sql), nl
    )).

check_web_app :-
    writeln("broken").

sql_ddl_create_table(_, _) :-
    writeln("broken").

% -------------------- helper

actual_expected(Act, Exp) :- Act == Exp, !.
actual_expected(Act, Exp) :-
    write("------------------------------ assertion failure"), nl,
    write("actual   : "), pretty(Act), nl,
    write("expected : "), pretty(Exp), nl,
    fail.

pretty(A) :- print_term(A, []).
