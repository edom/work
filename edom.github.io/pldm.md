---
title: Programming language design mistakes
permalink: /pldm.html
date: 2018-08-29 00:15 +0700
---

- TOC
{:toc}

## Disclaimer

This will change without notice.

This may contain mistakes.

Opinions may change.

## Justifying the creation of yet another programming language

A new programming language should fix unforeseen design mistakes in existing programming languages.
It should not repeat their design mistakes.
Therefore we should collect the design mistakes so that we can avoid repeating them.

What is a mistake?
How do we know it's a mistake?

We realize something is a mistake when we find a better way.

## Common design mistakes

### Ignoring programming language theory and research

Some mistakes are because the language designer is ignorant or too lazy.

### Dynamic typing

Dynamic typing is a design mistake.
Type inference goes back to 1958.
ML has type inference since 1973.
It's 2018.
There is no excuse for ignoring 60 years of research.
See [WP1](https://en.wikipedia.org/wiki/Type_inference#Hindley%E2%80%93Milner_type_inference_algorithm),
[WP2](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#History_of_type_inference).

### Lack of metaprogramming support

Lack of metaprogramming support is a design mistake.

### Lack of symbol overloading (ad-hoc polymorphism)

Scheme and Haskell lack convenient symbol overloading.
Haskell requires you to make a type class for ad-hoc polymorphism.

Without overloading, clashing names need to be prefixed manually.

### Mutable by default

Having variables mutable by default is a design mistake.

### Non-first-class constructs

Object-oriented programming is a design mistake.

- `a.b()` should not mean "call method b of object a"
- `a.b()` should mean "get the function a.b, then call it".
- `a.b()` should mean `(a.b)()`.
    - `m = a.b; m();` should mean the same as `a.b()`.
    - Functions should be first-class.
    Methods should not exist.
    Fields are enough (with function types).
    It should be possible to assign to methods.

Statements are a design mistake.
They should not exist.
Only expressions should.
[Xtend](https://www.eclipse.org/xtend/documentation/203_xtend_expressions.html)
and
[Haxe](https://code.haxe.org/category/principles/everything-is-an-expression.html)
do this right.
- Can we replace statements with expressions in existing imperative languages without breaking backward compatibility?
- But C has [WP:Comma operator](https://en.wikipedia.org/wiki/Comma_operator)?
- We can use lambda calculus for imperative programming.
    - Treat the statement separator (here we use semicolon `;`)
    as an operator (function with infix syntax)
    that sequences the effects of two expressions.
        - For example:
            - Let `a` be an expression.
            - Let `b` be an expression.
            - Thus `a;b` is an expression.
            - The value of `a;b` is the value of `b`.
            - The effect of `a;b` is the effect of `a` followed by the effect of `b`.
        - Semicolon is associative: `(a;b);c = a;(b;c)`. This also holds for the effects.

### Virtual machines

Virtual machines are a design mistake.
Instead, do semantic mapping: map a program in language A to an equivalent program in language B while preserving the meaning.

### More than one way to do something

["There is more than one way to do it"](https://en.wikipedia.org/wiki/There%27s_more_than_one_way_to_do_it) is a design mistake.

## Java

- Checked exceptions don't play nice with java.util.Stream.
Either checked exception or java.util.Stream is a design mistake.
    - https://en.wikipedia.org/wiki/Effect_system
- What others think Java gets wrong
    - http://tech.jonathangardner.net/wiki/Why_Java_Sucks

### Overcomplicated Java Virtual Machine

JVM does too much.
JVM bytecode verification is the compiler's job, not the VM's.
Does JVM bytecode verification even add any security?
Isn't JNI a bigger security hole?

### Working against programmers

Java presumes that the programmer is not only untrustworthy but also an idiot.

Programmers make mistakes, but they aren't idiots.

- [Ask Reddit: Why do so many reddit users hate java? : programming](https://www.reddit.com/r/programming/comments/utqb/ask_reddit_why_do_so_many_reddit_users_hate_java/cutv1/)
    - "Java's solution to the problem of C++ allowing you to blow your foot off was to chop off your legs."

### Throwing away the good parts of C++

- Forbidding multiple inheritance is a design mistake.
- Java interfaces are a design mistake.
See 2015 Robert C. Martin article [Java interface considered harmful](http://blog.cleancoder.com/uncle-bob/2015/01/08/InterfaceConsideredHarmful.html).
- Implementing generics too late with type erasure.
[C# 2.0 introduced](https://docs.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-version-history)
generics, without type erasure, while maintaining backward compatibility,
by duplicating everything in System.Collections to System.Collections.Generic.
This is ugly, but less ugly than type erasure.
    - [SO:How are C# generics implemented?](https://stackoverflow.com/questions/11436802/how-are-c-sharp-generics-implemented)
        - [Generics are not templates](https://blogs.msdn.microsoft.com/ericlippert/2009/07/30/whats-the-difference-part-one-generics-are-not-templates/)

## C and C++

In the 1970s these were tolerable: memory was limited, tools didn't exist.
In 2018 these aren't tolerable.

- Unattainable standard.
    - [SO:Are there any fully conforming C++ implementations?](https://stackoverflow.com/questions/5574469/are-there-any-fully-conforming-c-implementations)
- Pile of workarounds.
- C++ compilation is abysmally slow. `#include <iostream>` expands to 10000 lines.
- C pointer declaration syntax for pointers and arrays is a design mistake.
- Conflating pointers and arrays.
- Leaving a lot of things undefined.
- Zero-terminated string is a design mistake.
- Parsing requires symbol resolution and type checking.
- Header files. They should be automatically generated from C files instead.
- Preprocessor works on text, not on C/C++ AST.

## Haskell

- Template Haskell
    - Instead, we should have a metaprogramming library, and generate hs files.
- Haskell compilation is slow.
- Haskell module system is a design mistake.
- GHC: If A depends on B, and B changes without changing API, then A still requires recompilation.

## what

A problem with current programming methodologies is that they don't capture the higher-level properties of software, such as the architecture.
For example, how do we write, in a way that the computer can exploit, this statement:
"The fields of class C correspond one-to-one with the columns of database table T."?

- Other people's experiences
    - https://www.quora.com/If-you-were-to-design-a-programming-language-what-other-programming-language-would-it-evolve-from-and-what-aspect-of-that-language-would-you-do-differently
    - [5 Mistakes in Programming Language Design ― Andreas Zwinkau](http://beza1e1.tuxen.de/articles/proglang_mistakes.html)
        - [Mistakes in programming language design \| Hacker News](https://news.ycombinator.com/item?id=1500665)
    - [What is the greatest design flaw you have faced in any programming language? - Software Engineering Stack Exchange](https://softwareengineering.stackexchange.com/questions/55047/what-is-the-greatest-design-flaw-you-have-faced-in-any-programming-language)
    - [Programming Language Design Rules](http://www.inquisition.ca/en/info/gepsypl/rules.htm)
    - 1999 Steele article "Growing a language" [html](http://www.catonmat.net/blog/growing-a-language-by-guy-steele/)
