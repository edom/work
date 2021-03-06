/*
A type system that is based on how business users think about a system.

Several things must be distinguished:
    - a type equation, defined by type_definition/2
    - a type expression, that is the second argument of type_definition/2
*/

% ==================== input interface

/** type_definition(?TypeName,?Definition) is nondet.
    type_definition(++TypeName,-Definition) is semidet.

User-defined type.

TypeName is an atom.

Definition is a type expression, which is any of these:
    - =|#natural|=: a natural number.
    - =|#integer|=: an integer.
    - =|#string|=: a character string.
    - =|#string(N)|=: a character string with representation size limit in bytes.
    - =|#optional(T)|=, where T is a type expression.
    - A TypeName of another user-defined type.

The relation must be a function.
The same Type must not be defined more than once.

This is private to the model and this file.
Only the model may construct type expressions directly.
Model transformers must use the deconstructor/matcher predicates
such as type_natural/1.
*/

/** type_maxbitcount(?TypeName,?MaxBitCount) is nondet.
    type_maxbitcount(++TypeName,-MaxBitCount) is semidet.
    type_maxbytecount(?TypeName,?MaxByteCount) is nondet.
    type_maxbytecount(++TypeName,-MaxByteCount) is semidet.

Refinement for implementation.
*/

:- multifile type_definition/2.
:- multifile type_maxbitcount/2.
:- multifile type_maxbytecount/2.

:- include("../../syntax.pro").

% ==================== output interface

% -------------------- deconstruction, pattern-matching

type_natural(T) :- type_hnf(T, #natural).

type_integer(T) :- type_hnf(T, #integer).

type_natural_bit(T, N) :- type_natural(T), type_maxbitcount(T, N).

type_integer_bit(T, N) :- type_integer(T), type_maxbitcount(T, N).

type_identifier(T) :- type_hnf(T, #identifier).

type_identifier_bit(T, N) :- type_identifier(T), type_maxbitcount(T, N).

type_string(T) :- type_hnf(T, #string).

type_string_byte(T, N) :- type_hnf(T, #string(N)).
type_string_byte(T, N) :- type_string(T), type_maxbytecount(T, N).

type_optional(T, A) :- type_hnf(T, #optional(A)).

% -------------------- record type

recordtype_fields(Name, Fields) :- type_definition(Name, #record(Fields)).

/** recordtype_field(?TypeId,?FieldId,?FieldName,?FieldType) is nondet.
*/
recordtype_field(Type, Field, FieldName, FieldType) :-
    recordtype_field(Type, Field),
    field_name(Field, FieldName),
    field_type(Field, FieldType).

/** recordtype(?TypeName) is nondet.
    recordtype(++TypeName) is semidet.
*/
recordtype(Name) :- recordtype_fields(Name,_).

recordtype_field(RecType,Field) :-
    recordtype_fields(RecType,Fields),
    member(Name:Type,Fields),
    Field = [name-Name,type-Type].

field_name(F,N) :- member(name-N,F).
field_type(F,T) :- member(type-T,F).

/** type_natural(++TypeName) is semidet.
    type_integer(++TypeName) is semidet.
    type_integer_bit(++TypeName,-Bit) is semidet.
    type_identifier(++TypeName) is semidet.
    type_identifier_bit(++TypeName,-Bit) is semidet.

Pattern-matching type_definition/2.
*/

% ==================== internal

% -------------------- reduction

/** type_hnf(+TypeExp,-HeadNormalForm) is semidet.

Reduce a type expression to a head-normal form.

Refinements such as type_maxbitcount/2 are not included in the normal-form.
*/
type_hnf(A, _) :- \+ground(A), !, instantiation_error(A).

type_hnf(A, B) :-
    type_reduce(A, R),
    !,
    type_hnf(R, B).

type_hnf(A, A).

type_reduce(A, B) :- type_definition(A, B).
