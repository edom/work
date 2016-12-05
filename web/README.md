# Assorted work in progress

## Featured

* Gradle plugin for proper dependency resolution
    * [java-build/plugin/gradle](java-build/plugin/gradle)
* Locking across different processes
    * [concurrent/lock](concurrent/lock)
    * [concurrent/lock-client](concurrent/lock-client)
    * [concurrent/lock-server](concurrent/lock-server)
* Minimalistic Javadoc stylesheet
    * [javadoc.css](javadoc.css)
        * I made this stylesheet because Java 8's default stylesheet
        uses monospaced font for parameter description.

## Other documents

* [How to use](usage.md)
* [List of things brewing in this repository](list.md)
* [Javadoc](https://edom.github.com/java-doc/index.html) for everything in this repository
* [Design principles](design.md)

### Generating pom.xml files

To regenerate all Maven pom.xml files, use this custom Gradle invocation.
Note: this overwrites all those files, so don't edit those files manually.

```
gradle generatePom
```

The generated pom.xml files can be used to build the project
and install it to the local repository with Maven 3
by running this from the project root directory:

```
mvn clean install
```

Other commands such as `mvn compile` also work.

Sometimes you may have to clean up a bit:

```
rm -r ~/.m2/repository/com/spacetimecat
```

To delete all pom.xml files,
cd to the project root and run this (dangerous):

```
find -name pom.xml -delete
```

## Legal matter

(c) 2016 Erik Dominikus

This project is licensed under the Apache License Version 2.0.
