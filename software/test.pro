% Run main/0 to run all automatic tests.

% -------------------- things to be tested

:- import(file("sketch_browser.pro"),[
    browser/1
]).
:- import(file("test_auto.pro"),[
    test/1 as test_auto
    , run/1 as run_test_auto
]).
:- import(file("test_manual.pro"),[
    test/1 as test_manual
    , run/1 as run_test_manual
]).

test(auto:A) :- test_auto(A).
test(manual:A) :- test_manual(A).

run(auto:A) :- run_test_auto(A).
run(manual:A) :- run_test_manual(A).

run(manual:_, skip) :- !.
run(Name, ok) :- run(Name), !.
run(_, fail) :- !.

test_position(Name, Index/Total) :-
    findall(A, test(A), Names),
    length(Names, Total),
    nth1(Index, Names, Name).

:- dynamic known/1.

test_result(Name, Result) :-
    test(Name), known(test_result(Name,A)),
    A = Result.

test_result(Name, Result) :-
    test(Name), \+ known(test_result(Name,_)),
    run(Name, A),
    assertz(known(test_result(Name,A))),
    A = Result.

% -------------------- imperative

main :-
    format("~`=t~30| Test began at ~@.\n", [show_time]),
    retractall(known(_)),
    run_tests,
    summarize,
    format("~`=t~30| Test ended at ~@.\n", [show_time]).

%   Sometimes main/0 is reserved.

test_main :- main.

show_time :-
    current_output(Out),
    get_time(Time),
    format_time(Out, '%F %T %Z', Time).

run_tests :-
    forall(test_position(Test, Index/Max), (
        format("~`-t~30| [~w/~w] ~w\n", [Index,Max,Test]),
        test_result(Test, Result),
        report(Test, Result)
    )).

report(Test, skip) :- !, format("~`-t~30| \e[1mskip\e[22m: ~w\n",[Test]).
report(Test, fail) :- !, format("~`-t~30| \e[1mfail\e[22m: ~w\n",[Test]).
report(_, _) :- !.

summarize :-
    count(test(_), Count),
    count(test_result(_, skip), SkipCount),
    count(test_result(_, fail), FailCount),
    format("~`=t~30| Summary\n", []),
    format("Number of tests         : ~w\n", [Count]),
    format("Number of skipped tests : ~w\n", [SkipCount]),
    format("Number of failed tests  : ~w\n", [FailCount]),
    format("~`=t~30| Failed tests\n", []),
    (FailCount =< 0
    ->  write("There are no failed tests at this time,\n"),
        write("but this should not be taken to mean that the software does not contain errors.\n")
    ;   forall(test_result(Test,fail), (
            test_position(Test, Pos),
            format("[~w] ~w\n", [Pos,Test])
        ))
    ).

count(Goal, Count) :-
    F = count(0), (
        call(Goal),
        arg(1, F, M),
        N is M + 1,
        nb_setarg(1, F, N),
        fail
    ;   F = count(Count)
    ).
