/*
The canonical predicate is spo/3.
Everything else is for human writer convenience.
*/

/*
How do business users think of what exists?
What is their implicit ontology?

Example statements:

    - Positing the existence of ideal objects, declaring by fiat:
        - There exists a concept of employees.
        - There exists a concept of departments.
    - Declaring that an object belongs to a class:
        - "John" is an employee.
        - "Engineering" is a department.
    - Declaring that a class has an attribute:
        - An employee has a name.
        - A department has a name.
    - Declaring relationships:
        - An employee belongs to a department.
        - A department contains several employees.
    - Specifying an attribute:
        - The name of an account is a character string.
    - Constraints
        - Prices should not be negative.
        - Some people should not have access to something.

May an attribute have an independent existence from the object that has that attribute?
This is the "problem of universals".

Kinds of relations:

    - A is _detailed_ by B iff B cannot exist independently of A

concept(employee).

spo(each(employee), has(1), name).
spo(property(employee,name), is_a, string).
spo(property(employee,name), max_byte_count, 16).

concept_attribute_type(employee, name, #string).
*/

:- multifile
    concept/1.

% -------------------- canonical

/** spo(?Subject,?Predicate,?Object) is nondet.
    opv(+Object,+PropName,-PropValue) is nondet.

`spo` stands for Subject-Predicate-Object.

`opv` stands for Object-Property-Value.

The file ontology_subsume.pro describes a way to make spo/3 subsume (contain) opv/3.

This is similar to RDF triples but with modest purposes and narrow semantics.

Why should we write `spo(john, eat, rice)`
instead of `eat(john, rice)` or `john_eat(rice)`?

The reason is that spo/3 or opv/3 can be used to _generate_ both `eat(john, rice)` and `john_eat(rice)`.

Similar concepts:

    - subject-predicate-object in Semantic-Web RDF
    - object-property-value in philosophical ontology
    - entity-attribute-value in computer programming
*/

:- multifile
    spo/3,
    opv/3.

/** object(?ObjectId) is nondet.
    object_property(?ObjectId,?PropName) is nondet.
    object_property_is_optional(+ObjectId,+PropName) is semidet.

The original goal is to avoid repeating and scattering
missing-property checks all over the code base.

An object has identity.
Two objects may have the same properties,
but if their ObjectId differs, then they are not identical.

A property may be multi-valued.

Usage:

    - Load this file with include/1. Do not use consult/2 or use_module/2.
    - Run check_ontology/0 some time before using the ontology.
*/

:- multifile
    object/1,
    object_property_is_optional/2.

% -------------------- convenience

/** subject_predlist(?Subject,?PredList) is nondet.
    object_proplist(?Object,?PropList) is nondet.
*/
:- multifile
    subject_predlist/2,
    object_proplist/2.

:- include("ontology_check.pro").
