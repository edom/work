# Gradle plugin for dependency constraint intersection

## The problem

Gradle can't compute the intersection of dependency constraints.

### Explanation

Suppose that your project depends on A and B.

Suppose that both A and B depend on C.

Let A depends on C [1,3] and B depends on C [2,4].

If Gradle encounters B first while resolving dependencies,
Gradle will pick C version 4, silently breaking A.
If you use `failOnVersionConflict`,
Gradle will ask you to force the version of C.
Both require manual intervention.

## The solution

This plugin computes the intersection
of all dependency constraints of a project
before Gradle fixes the ranges into specific versions.
In the above case, this will infer that
your project depends on C [2,3],
and then Gradle will pick C version 3.

### Caveat

We don't know how this plugin interacts
with Gradle's dependency forcing and substitution features.

## How to use this plugin

Note: We will change this.

Clone the project.

Run `gradle install`. This will install the plugin to your local Maven repository.

Add the following fragment to your root build.gradle:

```
buildscript {
    repositories {
        mavenLocal()
    }
    dependencies {
        classpath 'com.spacetimecat.build.plugin.gradle:gradle:0.0.0-SNAPSHOT'
    }
}
```

Add the following fragment in your build.gradle project blocks.
Note that the order of applying plugins matters.
We don't know if it will work if you apply it before applying the `java` plugin.

```
apply plugin: 'com.spacetimecat.build.plugin.gradle.DependencyResolutionPlugin'
```

If you want to apply it to all projects, place this in your root build.gradle.
Make sure that the plugin is not applied before the `java` plugin.

```
allprojects {
    apply plugin: 'com.spacetimecat.build.plugin.gradle.DependencyResolutionPlugin'
}
```
