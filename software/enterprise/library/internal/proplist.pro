/** list_proplist(+List, -PropList) is det.

Create a property list from a list of Key-Value terms.

A key-value term is a '-'/2 functor term.

The user should always use this to construct a property list,
and not construct a list literal,
although the current implementation does nothing,

Should we have a separate "option list" concept?
An option list entry does not always have to be '-'/2.
An example is boolean options.
*/

list_proplist(List, List).

/** proplist_key_value(+List, ?Key, -Value) is nondet.
    proplist_key_value(+Options, +List, ?Key, -Value) is nondet.

List relates Key and Value.

List may also relate Key and many values.

Options is a list:

    - optional
    Do not throw/1 if Key is not in List.

    - default-Def
    Unify Value with Def if Key is not in List.
    Implies optional.

See also library(pairs).

Property lists are convenient for users but inconvenient for rule writers.
*/

proplist_key_value(List, Key, Value) :-
    member(Key-Value, List).

proplist_key_value(Opts, List, Key, _) :-
    must_be(ground, Opts),
    \+ member(optional, Opts),
    \+ member(default-_, Opts),
    \+ proplist_key_value(List, Key, _),
    !,
    existence_error(key, Key).

proplist_key_value(Opts, List, Key, Value) :-
    member(default-Def, Opts),
    \+ proplist_key_value(List, Key, _),
    !,
    Value = Def.

proplist_key_value(_, List, Key, Value) :-
    proplist_key_value(List, Key, Value).
