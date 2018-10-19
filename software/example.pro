:- module(example, [
    entity/2
    , main/2
    , test_java_syntax/1
]).

:- use_module('data.pro').

entity(todo, [
        [id, int32]
        , [text, varchar(1024)]
        , [what, int32, nullable]
    ]).

main(Class, Ast) :- java_class_ast(Class, Ast).

:- use_module('java_syntax.pro').

test_java_syntax(Tree) :- true
    , string_codes("1 + 2", Source)
    , phrase(expression(Tree), Source, [])
    .
