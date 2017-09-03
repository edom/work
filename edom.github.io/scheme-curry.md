---
title: Scheme currying
permalink: /scheme-curry.html
date: 2017-09-04 02:25:00 +0700
---

## Abstract

We define the macros `lambda/c`, `apply/c`, and `define/c`,
which are curried analogs of `lambda`, `apply`, and `define`, respectively.
This way is simple,
but it uses macros
and it is limited to fixed-arity lambda expressions.

## The idea

The idea of `lambda/c` is this pattern:

```
(lambda/c (a) z) => (lambda (a) z)
(lambda/c (a b) z) => (lambda (a) (lambda (b) z))
(lambda/c (a b c) z) => (lambda (a) (lambda (b) (lambda (c) z)))
...
```

The idea of `apply/c` is this pattern:

```
(apply/c a b) => (a b)
(apply/c a b c) => ((a b) c)
(apply/c a b c d) => (((a b) c) d)
...
```

However, there is a difference:
`apply` is an arity-2 procedure taking a procedure and a list,
whereas `apply/c` is a macro that takes one or more arguments.

The idea of `define/c` is this parallel:

```
(define (a b ... y) z) = (define a (lambda (b ... y) z))
(define/c (a b ... y) z) = (define a (lambda/c (b ... y) z))
```

## The code

I tried this code on Guile 2.0.9 on Ubuntu 14.04.

```
(define-syntax lambda/c
    (syntax-rules ()
        (
            (_ (x) body ...)
            (lambda (x) body ...)
        )
        (
            (_ (x y ...) body ...)
            (lambda (x) (lambda/c (y ...) body ...))
        )
    )
)

(define-syntax apply/c
    (syntax-rules ()
        (
            (_ f)
            (f)
        )
        (
            (_ f x)
            (f x)
        )
        (
            (_ f x y ...)
            (apply/c (f x) y ...)
        )
    )
)

(define-syntax define/c
    (syntax-rules ()
        (
            (_ (f) body ...)
            (define f (lambda () body ...))
        )
        (
            (_ (f x ...) body ...)
            (define f (lambda/c (x ...) body ...))
        )
    )
)
```

## Example

After the above macros have been defined
(by copying them to your REPL, for example),
the following fragment should print three threes.

```
(define f (lambda/c (x y) (+ x y)))
(define g (lambda (x) (lambda (y) (+ x y))))
(define/c (h x y) (+ x y))
(apply/c f 1 2)
(apply/c g 1 2)
(apply/c h 1 2)
```
