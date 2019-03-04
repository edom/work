:- op(2,fx,#).

:- multifile(type_definition/2).

% specification
:- consult('./accounting.pro').

% translation from specification to implementation
:- consult('./translation.pro').
