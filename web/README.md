# Assorted work in progress

## Featured

* Gradle plugin for proper dependency resolution:
[java-build/plugin/gradle](java-build/plugin/gradle)
* Locking across different processes:
[concurrent/lock](concurrent/lock),
[concurrent/lock-client](concurrent/lock-client), and
[concurrent/lock-server](concurrent/lock-server).

## Other things

* Generic server using entrances and ushers:
[server](server)
* Web application library:
[web](web)

## How to use

### Clone the repository

If you are using SSH:

```
git clone git@github.com:edom/web.git
```

If you are not:

```
git clone https://github.com/edom/web.git
```

### Generate IntelliJ IDEA files

The build script has been tested with Gradle 3.1,
but should also run with Gradle 2.9.

```
gradle idea
```

After that, you can open the source in IntelliJ IDEA.

### Deploy everything

The best way to do this is by repository manager
like Sonatype Nexus or JFrog Artifactory,
but if you don't mind installing to your local repository, you can:

```
gradle install
```

On my machine with default settings,
the Maven local repository is at `~/.m2/repository`.

### Using the libraries

Add the dependencies to your build.gradle.

```
dependencies {
    compile "com.spacetimecat.GROUP:ARTIFACT:VERSION"
}
```

### See the documentation

We also have the [Javadoc](https://edom.github.com/java-doc/index.html)
for everything in this repository.

More information are spread in `README.md` files in various directories.
You can browse this directly through GitHub.

## Design principles

* Use Semantic Versioning.
* Don't invert control.
The user defines the main class and calls the library,
not the other way around.
* Minimize magic so that everyone can use an IDE
to browse and understand the code.
This means avoiding annotations and reflection.
* Work with and build on existing codes and standards
instead of trying to replace them.
* When in doubt, choose simplicity.
* Progressive enhancement instead of graceful degradation.
* None of those principles are permanent.

## Legal matter

(c) 2016 Erik Dominikus

This project is licensed under the Apache License Version 2.0.
