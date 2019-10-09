# Rackbol internals

A _model_ can be thought of as a description of a system.

A _translator_ converts a model into an implementation.

The key idea is:
The same _syntax_ represents the same _intention_,
but the _meaning_ depends on the interpreter.
That is, the syntax
For example, from the following "add" procedure definition,
a translator may produce a console application with getline-style input,
and another translator may produce a web application serving with HTML-form input:

```
(DEFINE PROCEDURE (add x y)
    (+ x y)
)
```

In all implementations, the intention is the same:
To make the machine able to accept commands for adding two numbers.
It is less important whether the app is console, web, or mobile.
The most important thing is that the app does what is intended.

A computer programmer is a human that translates human intentions to computer code.

But a translator may be a human, a machine, or a human-machine combination.

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

The translators.
[web.rkt](web.rkt) uses continue and db for typical web applications.
Everything exported by this module will be available by default
in all `#lang rackbol/web` modules.
