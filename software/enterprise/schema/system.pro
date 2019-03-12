:- module(schema_system,[
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

/** <module> Schema for a system

A _business logic_ is represented as a procedure/1.

Access control is application logic, not business logic.

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



% -------------------- type



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



% -------------------- procedure



/** procedure(?ProcId) is nondet.
    procedure_name(?ProcId,?Name) is nondet.
    procedure_check(?ProcId,?CheckExp) is nondet.
    procedure_input(?ProcId,?InputName,?InputType) is nondet.
    procedure_output(?ProcId,?OutputName,?OutputType) is nondet.
    procedure_action(?ProcId,?Action) is nondet.

A _procedure_ has name, checks, inputs, outputs, and action.

Some semantics:
    - If any check fails, the action does not run.
    - The system asks the user for the inputs.
    - The action runs after all inputs are ready.
    - A procedure does not have _state_.
    A procedure belongs to a system that may have state.

Some translation ideas:
    - A procedure may translate to several web pages.
    - The input types determine the HTML form.

Action is a statement in the _Procedure Action Language_ (PAL).

The principle of PAL is to think about how the _end-user_ would describe what the system does.
For example, if the end-user thinks that the system stores data,
then PAL should have a primitive about data storage.
If the end-user does not care about something, then the PAL programmer should also not care about it.
For example, the end-user does not care about how the system actually stores data;
what the end-user cares about is that the same stored data is retrievable later.
Thus PAL should delegate the meaning of "to store data" to the translators,
but with the semantics that retrieving a stored data should exactly reproduce the stored data.
Thus, PAL constrains but does not determine the meaning of its primitives.

The end-users only care about whether the system meet their requirements.
They do not care about the implementation details.

Design issues:
    - Which paradigm is the most convenient for business logic: procedural, functional, logic, data-flow, what?
        - The language may be a lambda-calculus with bells and whistles such as relational algebra expressions.
            - 1995, Hillebrand, Kanellakis, & Mairson, "Database Query Languages Embedded in the Typed Lambda Calculus"
        - Should we use relational algebra?
            - Pro: Simplifies translation to SQL.
            - Con: No transitive closure, unless we bolt on a transitive-closure operator,
            but then the mapping to SQL would be complicated.
    - What is the difference between a type and a check?
    Aren't types for checking?
    - Should it be called a "procedure"? What should it be called?
        - task, user task
        - process, business process
        - activity
        - interaction
        - transaction, atomic interaction
        - function, functionality
        - Must a procedure have procedural style (as opposed to functional or relational)?
*/

:- multifile procedure/1,
             procedure_name/2,
             procedure_input/3,
             procedure_output/3.

/** function_definition(?Id,?Inputs,?Outputs,?Checks,?OutExps) is nondet.

Design problem: Should we have a functional language, or a procedural language?
*/
