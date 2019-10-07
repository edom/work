# Rackbol internals

These are the layers, from bottom to top:

[lang.rkt](lang.rkt):
This is Racket plus the "fewer-parentheses" forms such as λ, LET, and its variants.

[base.rkt](base.rkt):
This is lang.rkt + an object system + some `syntax/parse` syntax classes.

[library.rkt](library.rkt):
This exports forms such as DEFINE and FOR.
