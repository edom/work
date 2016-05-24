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
stack ghci
:m Jvm
testrunvm
```
