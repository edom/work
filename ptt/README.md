# Program transformation tool

## How do I generate the documentation?

```
stack haddock
```

The output directory should be:

```
.stack-work/install/x86_64-linux-gmp4/lts-6.26/7.10.3/doc/
```

## How do I set up vanilla vim?

By vanilla vim, we mean the vim you get by
`sudo apt-get install vim` from the official Ubuntu 14.04 repository
without customization (except copying the default vimrc
and changing a few default indentation options).

Make sure that you cd to the directory containing this readme first.

In the snippet below, `<Tab>` means you press the Tab key.

```
vim -S Session.vim
:set path=<Tab>,**
:set ai
:mksession!
```

## How do I browse the code with vanilla vim?

In the snippet below, `<file-pattern>`
is the name of the file you want to open.

```
:find <file-pattern>
:tabfind <file-pattern>
```

# Design constraints

- The compiler must not require the output program
to be linked with a runtime library like GHC RTS.

# Roadmap

## Where we are

- Normal-order beta-reduce lambda calculus terms

## Where we would like to be

- Parse the source code of a Haskell module using the `haskell-src-exts` package
- Typecheck the abstract syntax tree
- Interpret GHC `base` package by interpreting the GHC primitives
