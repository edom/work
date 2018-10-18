:- module(java, [
    java_primitive_reference_type/2
    , java_class_ast/2
]).

:- use_module('data.pro').

% This relates a primitive type and its corresponding reference type.

java_primitive_reference_type(Pri, Ref) :-
    member((Pri, Ref), [
        (short, 'java.lang.Short')
        , (int, 'java.lang.Integer')
        , (long, 'java.lang.Long')
        , (float, 'java.lang.Float')
        , (double, 'java.lang.Double')
    ]).

java_primitive_type(Pri) :- java_primitive_reference_type(Pri, _).

% The term 'public' seems to be special syntax.
% Removing the parenthesis causes error.
% http://www.swi-prolog.org/pldoc/man?section=declare
java_class_field(Class, Field_name, Java_type, Access, Final) :- true
    , entity_field(Class, Field_name, Sql_type, _Opts)
    , entity_field_nullable(Class, Field_name, Nullable)
    , sql_java_type(Sql_type, Java_type, Nullable)
    , Access = (public)
    , Final = final
    .

java_class_ast(Class, Ast) :-
    findall(Ast, java_ast_field(Class, _, Ast), Ast_fields)
    , Ast = class(Class, members(Ast_fields))
    .

java_ast_field(Class, Name, Ast) :- true
    , java_class_field(Class, Name, Type, Access, Final)
    , Ast = field(Access, Final, Type, Name)
    .
