This is very experimental.

The goal is to run the Org Mode source files that implement the Org Mode parser.
The bigger goal is to translate Org files into Scribble HTML outputs.

The main idea is to reuse Racket's reader as much as possible to read Emacs Lisp source texts:
Make a Racket readtable that makes Racket's "read" read Emacs Lisp sources as Racket values.

To run an Emacs Lisp file in Emacs:

```
emacs --quick --batch --script example.el
```

To run an Emacs Lisp file in Racket:

```
./racket.sh emacs-lisp/main.rkt example.el

# or

./racket.sh -l errortrace -t emacs-lisp/main.rkt example.el
```
