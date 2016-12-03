# How to use

## Clone the repository

If you are using SSH:

```
git clone git@github.com:edom/web.git
```

If you are not:

```
git clone https://github.com/edom/web.git
```

## Generate IntelliJ IDEA files

The build script has been tested with Gradle 3.1,
but should also run with Gradle 2.9.

```
gradle idea
```

After that, you can open the source in IntelliJ IDEA.

## Deploy everything

The best way to do this is by repository manager
like Sonatype Nexus or JFrog Artifactory,
but if you don't mind installing to your local repository, you can:

```
gradle install
```

On my machine with default settings,
the Maven local repository is at `~/.m2/repository`.

## Using the libraries

Add the dependencies to your build.gradle.

```
dependencies {
    compile "com.spacetimecat.GROUP:ARTIFACT:VERSION"
}
```
