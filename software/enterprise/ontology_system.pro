:- module(ontology_system,[
    type_definition/2
    , type_maxbitcount/2
    , type_maxbytecount/2
    , type_normalform/2
    , recordtype/1
    , recordtype_field/2
    , recordtype_fields/2
    , field_name/2
    , field_type/2
    , type_natural/1
    , type_integer/1
    , type_integer_bit/2
    , type_identifier/1
    , type_identifier_bit/2
    , type_string/1
    , type_optional/2
]).
:- use_module('./syntax.pro').
/** <module> Ontology for a system

Contents for language user:
    - Type definition
        - type_definition/2
    - Refinement
        - type_maxbitcount/2
        - type_maxbytecount/2
    - State definition
        - state/1
        - state_type/2
        - state_initializer/2

Contents for language designer:
    - Type reduction
        - type_normalform/2
    - Iterator
        - recordtype_fields/2
        - recordtype_field/2
        - recordtype/1
    - Pattern-matching
        - type_natural/1
        - type_integer/1, type_integer_bit/2
        - type_identifier/1, type_identifier_bit/2
        - type_string/1
        - type_optional/2
*/

/** type_definition(?TypeName,?Definition) is nondet.
    type_definition(++TypeName,-Definition) is semidet.

User-defined type.

TypeName is an atom.

Definition is a type expression, which is any of these:
    - =|#natural|=: a natural number.
    - =|#integer|=: an integer.
    - =|#string|=: a character string.
    - =|#optional(T)|=, where T is a type expression.
    - A TypeName of another user-defined type.

The relation must be a function.
The same Type must not be defined more than once.
*/
:- multifile type_definition/2.

/** type_maxbitcount(?TypeName,?MaxBitCount) is nondet.
    type_maxbitcount(++TypeName,-MaxBitCount) is semidet.
    type_maxbytecount(?TypeName,?MaxByteCount) is nondet.
    type_maxbytecount(++TypeName,-MaxByteCount) is semidet.

Refinement for implementation.
*/
:- multifile type_maxbitcount/2,
             type_maxbytecount/2.

/** state_type(?StateId,?Type) is nondet.

A state generalizes memories, global variables, files, databases.

A state is something that the system remembers.

A state may be either volatile or persistent?
*/
:- multifile state/1,
             state_type/2,
             state_initializer/2.

/** type_normalform(++TypeName,-NormalForm) is semidet.

Refinements such as type_maxbitcount/2 are not included in the normal-form.
*/
type_normalform(A,_) :- \+ground(A), !, instantiation_error(A).
type_normalform(A,B) :- type_reduce(A,R), !, type_normalform(R,B).
type_normalform(A,A).

    type_reduce(A,B) :- type_definition(A,B).

recordtype_fields(Name,Fields) :- type_definition(Name,#record(Fields)).

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

type_natural(T) :- type_normalform(T,#natural).

type_integer(T) :- type_normalform(T,#integer).

type_integer_bit(T,N) :- type_integer(T), type_maxbitcount(T,N).

type_identifier(T) :- type_normalform(T,#identifier).

type_identifier_bit(T,N) :- type_identifier(T), type_maxbitcount(T,N).

type_string(T) :- type_normalform(T,#string).

type_optional(T,A) :- type_normalform(T,#optional(A)).
