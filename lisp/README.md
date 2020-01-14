# Some Lisp programs

## Installation

Do this once in Bash:

```
sbcl --no-userinit --script install.lisp
ln -s -f "$PWD/sbclrc.lisp" ~/.sbclrc
```

## Loading

Load everything and interact:

```
rlwrap sbcl --noinform --load load.lisp
```
