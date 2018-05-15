---
title: Class-based programming
permalink: /cbp.html
date: 2018-05-15 23:54:00 +0700
---

We can think of a class as a blueprint of template of objects.

A class has name, fields, and methods.

Every object has a class.

In class-based programming, objects don't have methods.
Classes have instance methods.
Each instance method can be called with an instance as a hidden argument.

A class groups data and code that needs that data.

A class is a way of organizing your program.

From procedural point of view, these two are the same:

```
// Java

object.method(arg0, arg1, ...)

// C

method(object, arg0, arg1, ...)
```

The dot is just a syntax for passing an implicit first argument.

The dot operator associates to the left:

```
a.b().c().d() = ((a.b()).c()).d()
```
