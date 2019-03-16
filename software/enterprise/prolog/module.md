# A Prolog module system

## Usage

    - Declare each ontology with ontology/1 and the ontology property predicates.
    - Declare each module with module/1 and the module property predicates.
    - Declare which module instantiates which ontology with module_instantiates_ontology/2.
    - Declare linking.
    - Call load_ontologies/0.
    - Call instantiate_ontologies/0 to dynamically declare multifile predicates
    according to module_instantiates_ontology/2.
    - Call load_modules/0.
    - Call link_modules/0.

## Design

A _module_ is a namespace that contains predicates.

Practices for a clash-free module system:
    - A file must not name its own module.
    - A file must not contain any call with explicit module (phrase of the form M:F).
    If file U depends on M:F, then U must declare a multifile predicate F, and let the linker link U:F and M:F.

There are several ways module A can import module B:
    - Import every exported predicate. This assumes no name clash.
    - Explicit linking.

By "linking", we mean asserting a clause for a multifile predicate.
