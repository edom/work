:- module(example, [
    entity/2
    , main/2
]).

:- use_module('data.pro').

entity(todo, [
        [id, int32]
        , [text, varchar(1024)]
        , [what, int32, nullable]
    ]).

main(Class, Ast) :- java_class_ast(Class, Ast).
