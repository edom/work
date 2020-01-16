# Some Lisp programs

## Installation

If you are using Debian 9, download the prebuilt SBCL 1.5.5 binary because:
- The prebuilt SBCL 1.5.6 and later binaries [don't run on Debian 9](https://sourceforge.net/p/sbcl/mailman/message/36780098/).
- `ironclad` requires somewhat recent SBCL.

Then, assume:

```
SBCL=/mess/sbcl-1.5.5-x86-64-linux/run-sbcl.sh
```

Then, do this once in Bash:

```
$SBCL --no-userinit --script install.lisp
ln -s -f "$PWD/sbclrc.lisp" ~/.sbclrc
```

## Loading

This `QL:QUICKLOAD`s each `SYSTEM`, and interacts:

```
rlwrap $SBCL --noinform --load load.lisp SYSTEM...
```
