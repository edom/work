:- module(main,[]).
:- reexport('./syntax.pro').
% specification language definition
:- reexport('./type.pro').
:- reexport('./java.pro').
% translation from specification to implementation
:- reexport('./sql.pro').
:- reexport('./translation.pro').
:- reexport('./link.pro').

/** <module> Enterprise model

Usage:
    - Define types with type_definition/2.
    - Optional: Customize the mapping recordtype_javaclass/2.
    - Generate program with generate/0.
*/

:- load_specs([
    './accounting.pro'
    , './employee.pro'
]).
