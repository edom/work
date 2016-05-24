# Java bytecode interpreter

**Haji** is an acronym for **Ha**skell **J**ava bytecode **i**nterpreter.

This project is still at its infancy; it's a bug's nest.
However, it can run very simple programs.

# Generating the documentation

The documentation is also available [online](http://edom.github.io/haji-doc/index.html).

You can also generate the documentation for offline viewing:

```
git clone https://github.com/edom/haji.git
cd haji
stack haddock
```

# Demo

```
javac Hello.java
stack ghci
:m Jvm
testrunvm
```
