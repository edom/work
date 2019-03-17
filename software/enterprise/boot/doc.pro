/** <module> Source code documentation system
*/

/** documentation(?Thing,?Documentation) is nondet.

The documentation of Thing is Documentation.

Thing may be:

    - module(Module)

    - predicate(Module,Name,Arity)

Documentation syntax and semantics:

    - summary-Markup
    Markup is a markup.

    - detail-Markup
    Markup is a markup.

Markup:

    - List
    Concatenation.
    List is a list of Documentation.

    - string(String)
    Verbatim output.
    String is a string/1.

    - link(Thing)
    Internal link to Thing.

Documentation is designed for clarity of semantics and translatability to other formats,
not for convenience of authoring.
*/

:- multifile documentation/2.
:- dynamic documentation/2.

documentation_property(Thing, Key, Value) :-
    documentation(Thing, List),
    member(Key-Value, List).

/** thing_summary(?Thing, -Summary) is det.
*/

thing_summary(Thing, Summary) :- documentation_property(Thing, summary, Summary), !.
thing_summary(_, empty).

:- include("imperative.pro").
:- include("internal/doc_browse.pro").