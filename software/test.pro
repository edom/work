% -------------------- things to be tested

:- consult_unregistered_into_module("html_cphe.pro", html_cphe).
:- consult_unregistered_into_module("sketch_application.pro", sketch_application).

% -------------------- the tests themselves

test(html_cphe_attribute) :-
    Exp = p(a=b, "T", c=d, b("U", "V")),
    html_cphe:cphe_ast(Exp, Ast),
    pretty(Exp), nl,
    pretty(Ast), nl,
    actual_expected(
        Ast,
        elem(p,[attr(a,b),attr(c,d)],[text("T"),elem(b,[],[text("U"),text("V")])])
    ).

test(html_cphe_string) :-
    Exp = a(href="foo.png", "The image"),
    html_cphe:cphe_string(Exp, Str),
    pretty(Exp), nl,
    write(Str), nl.

test(html_cphe_string_empty_tag) :-
    Exp = img(src="foo.png", alt="bar"),
    html_cphe:cphe_string(Exp, Str),
    pretty(Exp), nl,
    write(Str), nl.

test(html_cphe_document) :-
    Exp = ['!doctype'(html), html(head(title("Title")), body(p("Paragraph")))],
    html_cphe:cphe_string(Exp, Str),
    pretty(Exp), nl,
    write(Str), nl.

% -------------------- running the tests

actual_expected(Act, Exp) :- Act == Exp, !.
actual_expected(Act, Exp) :-
    write("------------------------------ assertion failure"), nl,
    write("actual   : "), pretty(Act), nl,
    write("expected : "), pretty(Exp), nl,
    fail.

test_name(Name) :- clause(test(Name), _).
test_names(Names) :- findall(Name, test_name(Name), Names).

pretty(A) :- print_term(A, []).

test_all :-
    test_names(Names),
    length(Names, Count),
    test_all(1, Count, Names, Fails),
    length(Fails, FailCount),
    write("------------------------------ summary\n"),
    format("Number of tests        : ~w\n", [Count]),
    format("Number of failed tests : ~w\n", [FailCount]),
    write("------------------------------ failed tests\n"),
    forall(member(Index-Name,Fails),
        format("[~w] ~w\n", [Index,Name])
    ).

test_all(_, _, [], []) :- !.
test_all(Index, Count, [Name|Names], Fails) :- !,
    format("------------------------------ [~w/~w] ~w\n", [Index,Count,Name]),
    run_test(Name, Ok),
    report_test(Name, Ok),
    Index1 is Index + 1,
    test_all(Index1, Count, Names, Fails0),
    (Ok = true
    ->  Fails = Fails0
    ;   Fails = [Index-Name|Fails0]
    ).

report_test(Name, false) :- !,
    format("------------------------------ \e[1mfail\e[22m: ~w\n",[Name]).

report_test(_, _) :- !.

run_test(Name, true) :- test(Name), !.
run_test(_, false) :- !.
