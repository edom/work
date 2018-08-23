---
title: Class-based programming
permalink: /cbp.html
date: 2018-05-15 23:54:00 +0700
---

- What is a class?
    - What is a class?
        - A class is a blueprint (a template).
        - A class is a way of organizing your program.
    - What does a class have?
        - A class has name, fields, and methods.
    - What does a class do?
        - A class groups data and code that needs that data.
- Every object belongs to a class.
- In class-based programming:
    - Objects don't have methods.
    - Classes have instance methods.
    - Each instance method can be called with an instance as a hidden argument.
- From procedural point of view, these two are the same:

```
// Java

object.method(arg0, arg1, ...)

// C

method(object, arg0, arg1, ...)
```

- The dot is just a syntax for passing an implicit first argument.
- The dot operator associates to the left:

```
a.b().c().d() = ((a.b()).c()).d()
```
