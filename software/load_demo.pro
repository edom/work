:- annotate(file,[
    purpose-"test loader"
    , problem-"need more convincing demo"
    , tags-[
        architecture
        , demo
        , example
        , test
    ]
]).

:- export([
    hello/0
    , goodbye/0
    , my_throw/0
]).

:- include("load_demo_include.pro").
:- import(file("load_demo_import.pro"),[
    data/1
    , hi/0
]).

:- annotate([
    purpose-"say hello"
]).
hello :-
    format('~w~n',['Hello world.']).

test_meta_predicate :-
    call(hi),
    call(hello),
    call(goodbye),
    assertz(data(test)).

test_call(A) :- call(A).

:- section("test shadowing").

    member(Elem, Array) :-
        arg(_, Array, Elem).

    test_shadow :-
        forall(member(A, array(1,2,3)),
            writeln(A)).

:- end_section.

:- section("test exception stack trace").

    my_throw :-
        throw(error(test,_)).

:- end_section.
