/** what_is_a_module is det.

A _module_ is a namespace that contains predicates.

Practices for a clash-free module system:
    - A file must not name its own module.
    - A file must not contain any call with explicit module (phrase of the form M:F).
    If file U depends on M:F, then U must declare a multifile predicate F, and let the linker link U:F and M:F.

There are several ways module A can import module B:
    - Import every exported predicate. This assumes no name clash.
    - Explicit linking.

By "linking", we mean asserting a clause for a multifile predicate.
*/

what_is_a_module.