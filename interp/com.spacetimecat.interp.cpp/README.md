# Interpreter

(Need cool examples.)

```
1 2 +
```

## Technicalities

Features (or problems, depending on your point of view):

- naïve run-time type system
- recursive descent parsing
- garbage collection, naïve mark-and-sweep
- dynamically scoped
- like lisp but postfix?
- like postscript
- non-blocking runtime?
- multi-precision integer arithmetics with GMP (optional)
- GNU readline (optional)

Non-orthodoxies:

- simple build system?
- #include cpp file?
In 2019, for 1000-line projects,
including everything into one cpp file is still fast enough.

## Building

Requirements:

- bash
- g++
- libraries
    - gmp
    - readline
- Debian 9 ("Stretch")

See [build.sh](build.sh).
