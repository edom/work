/** <module> Schema for a system

A _business logic_ is represented as a procedure/1.
For example, access control is application logic, not business logic.

## Usage for users

Define types using type_definition/2.

Refine types with:
    - type_maxbitcount/2
    - type_maxbytecount/2

Define states with state/1 and friends.

Globalization can be done with term_locale_string/3.

## Usage for translators

Normalize types with type_normalform/2.

Enumerate record types with recordtype/1 and friends.

Pattern-match on types with:
    - type_natural/1
    - type_integer/1, type_integer_bit/2
    - type_identifier/1, type_identifier_bit/2
    - type_string/1
    - type_optional/2
*/

:- include("internal/type.pro").

% -------------------- system, state, procedure

/** state_type(?StateId,?Type) is nondet.

A state generalizes memories, global variables, files, databases.

A state is something that the system remembers.

A state may be either volatile or persistent?
*/

/** procedure(?ProcId) is nondet.
    procedure_name(?ProcId,?Name) is nondet.
    procedure_check(?ProcId,?CheckExp) is nondet.
    procedure_input(?ProcId,?InputName,?InputType) is nondet.
    procedure_output(?ProcId,?OutputExp) is nondet.

This tries to be as close as possible to the end-user's mental model of what the system does.

The Procedure Action Language (PAL) is documented elsewhere:

    - OutputExp is described in ../language/pal_usage.md.
    - The design is described in ../language/pal_design.md.

Some translation ideas:

    - A procedure may translate to several web pages.
    - The input types determine the HTML form.
*/

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
