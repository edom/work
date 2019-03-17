/** object(?ObjectId) is nondet.
    object_property(?ObjectId,?PropName) is nondet.
    object_property_value(+ObjectId,+PropName,-PropValue) is nondet.
    object_property_is_optional(+ObjectId,+PropName) is semidet.

This is similar to RDF triples but with modest purposes and narrow semantics.

The original goal is to avoid repeating and scattering
missing-property checks all over the code base.

An object has identity.
Two objects may have the same properties,
but if their ObjectId differs, then they are not identical.

A property may be multi-valued.

Usage:

    - Load this file with include/1. Do not use consult/2 or use_module/2.
    - Run check_ontology/0 some time before using the ontology.

Similar concepts:

    - subject-predicate-object in Semantic-Web RDF
    - object-property-value in philosophical ontology
    - entity-attribute-value in computer programming
*/

:- multifile
    object/1,
    object_property_value/3,
    object_property_is_optional/2.

% -------------------- checking

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
    \+ object_property_value(Object, Prop, _),
    \+ object_property_is_optional(Object, Prop).
