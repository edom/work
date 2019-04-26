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

:- annotate([
    purpose-"say hello"
]).
hello :-
    format('~w~n',['Hello world.']).
