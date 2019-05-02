# Old documentation

This has been merged into `../meta`.
This documentation no longer applies.
This documentation should be deleted.

# Haskell Java Interoperation

**Haji** is an acronym for **Ha**skell **J**ava **i**nteroperation.

## Java bytecode interpreter

This project is still at its infancy; it's a bug's nest.
However, it can run very simple programs.

# Generating the documentation

To generate documentation for offline viewing:

```
git clone https://github.com/edom/work.git
cd work
cabal new-haddock haji
```

# Demo

Do this once:

```
cd <PROJECT-PATH>
( mkdir -p jre/lib/rt; cd jre/lib/rt; jar xf <PATH-TO-RT-JAR> )
```

where `<PROJECT-PATH>` is the path to the `haji` directory and `<PATH-TO-RT-JAR>` is the path to `rt.jar`.
This assumes that you have installed a Java Runtime Environment like OpenJDK JRE.
To find where `rt.jar` is, you can use `locate`:

```
locate '*/rt.jar'
```

Then try running `Hello.class`:

```
javac Hello.java
cabal new-repl haji
:m Jvm
testrunvm
```
