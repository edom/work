# Rackbol internals

These are the layers, from bottom to top:

[racket-base.rkt](racket-base.rkt):
This is the only module allowed to require `racket/base`:
Other modules must not use `racket/base` directly.
This exports `racket/base` and `require+provide`.

[lang.rkt](lang.rkt):
This exports `racket-base.rkt`,
`syntax/parse`, related syntax libraries,
some libraries in the `racket` collection,
and the fewer-parentheses forms such as λ, LET, and its variants.

[base.rkt](base.rkt):
This exports `lang.rkt`,
`others.rkt`,
an object system,
and some `syntax/parse` syntax classes.

[main.rkt](library.rkt):
This exports forms such as DEFINE and FOR.
Everything exported by this module will be available by default in all `#lang rackbol` modules.
