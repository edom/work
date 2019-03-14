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

    % -------------------- behavior

    , procedure/1

    % -------------------- globalization

    , term_locale_string/3
]).

:- use_module('./syntax.pro').

/** <module> Schema for a system

A _business logic_ is represented as a procedure/1.

Access control is application logic, not business logic.

Define types using type_definition/2.

Refine types with:
    - type_maxbitcount/2
    - type_maxbytecount/2

Define states with state/1 and friends.

Normalize types with type_normalform/2.

Enumerate record types with recordtype/1 and friends.

Pattern-match on types with:
    - type_natural/1
    - type_integer/1, type_integer_bit/2
    - type_identifier/1, type_identifier_bit/2
    - type_string/1
    - type_optional/2

Globalization can be done with term_locale_string/3.
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
    procedure_action(?ProcId,?ActionExp) is nondet.

Procedure Action Language (PAL).

ActionExp is described in pal_usage.md.

The design is in pal_design.md.

Some translation ideas:

    - A procedure may translate to several web pages.
    - The input types determine the HTML form.
*/

:- multifile procedure/1,
             procedure_name/2,
             procedure_input/3,
             procedure_output/3.

/** function_definition(?Id,?Inputs,?Outputs,?Checks,?OutExps) is nondet.

Design problem: Should we have a functional language, or a procedural language?
*/



% -------------------- globalization, internalization, localization



/** term_locale_string(?Term,?Locale,?String) is nondet.

Term is a Prolog functor that represents a String that gets substituted according to the Locale.

Locale is language or language-region.

String is a string/1.

Example:

==
term_locale_string(hello, eng-usa, "Hello, English speakers in the USA.").
term_locale_string(hello, eng-gbr, "Hello, English speakers in the UK.").
term_locale_string(hello, eng, "Hello, English speakers.").
term_locale_string(hello, ind-idn, "Halo, penutur Bahasa Indonesia di Indonesia.").
term_locale_string(hello, ind, "Halo, penutur Bahasa Indonesia.").
term_locale_string(hello, ind, "Halo, penutur Bahasa Indonesia.").

term_locale_string(are_you_going_to_buy(Count,Singular), eng, String) :-
    number_singular_inflected(Count, Singular, Inflected),
    format(string(String), "Are you going to buy ~w ~w?", [Count,Inflected]).

number_singular_inflected(Number, Singular, Inflected) :-
    Number = 1
    ->  Inflected = Singular
    ;   singular_plural(Singular, Inflected).

singular_plural(person, people).
singular_plural(shirt, shirts).
singular_plural(baby, babies).
==

You can use your own convention for Locale as long as you stick to one convention.
For example, language codes may be ISO 631-3 codes,
and country codes may be ISO 3166-1 alpha-3 codes.

See also:

    - language codes https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
    - country codes https://en.wikipedia.org/wiki/ISO_3166-1
*/

:- multifile term_locale_string/3.
