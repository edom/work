check_ontology :-
    debug(ontology, "ontology: Looking for missing properties", []),
    \+ object_property_missing(_,_),
    !.

check_ontology :-
    object_property_missing(Object, Prop),
    print_message(error, object_property_missing(Object, Prop)),
    fail.

prolog:message(object_property_missing(Object,Prop)) -->
    ["~w is missing required property ~w"-[Object,Prop]].

object_property_missing(Object, Prop) :-
    object(Object),
    object_property(Object, Prop),
    \+ opv(Object, Prop, _),
    \+ object_property_is_optional(Object, Prop).
