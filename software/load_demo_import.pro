:- annotate(file,[
    purpose-"test loader import"
]).

:- export([
    data/1
    , hi/0
]).

:- dynamic data/1.

hi :-
    format('~w~n',['Hi world.']).
