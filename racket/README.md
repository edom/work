Welcome to Erik Dominikus's software garden.

[emacs-lisp](emacs-lisp): Racket readtable for Emacs Lisp.

To run a file such as `test.rkt`:

```
./racket-env.sh racket test.rkt
```

To run `test.rkt` with stack trace on error:

```
./racket-env.sh racket -l errortrace -t test.rkt
```

To generate HTML documentation from `doc/main.scrbl` to `_out/main.html`:

```
./racket-env.sh racket generate-docs.rkt
```
