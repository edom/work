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
