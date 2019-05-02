# Maven plugins

## Requirements

Building the plugins requires
[Maven 3.2.2](https://maven.apache.org/docs/3.2.2/release-notes.html)
or newer due to
[MNG-5346](https://issues.apache.org/jira/browse/MNG-5346).

## Create an empty project

Create a `pom.xml` file in IntelliJ IDEA.

Copy this into the editor.

```
<?xml version="1.0" encoding="UTF-8" ?>
<project
xmlns="http://maven.apache.org/POM/4.0.0"
xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    <modelVersion>4.0.0</modelVersion>
</project>
```

Type the rest of the POM. IntelliJ IDEA will help you.

See also the [Maven POM reference](https://maven.apache.org/pom.html).

## Limit your Maven usage

I limit my Maven usage to these scenarios:

- specify dependencies for IntelliJ IDEA
- download all transitive dependencies
- upload my artifacts to remote repositories

I think Maven does those things very well.
I'm happy with the way it does them.

I'm happiest when I am not writing a plugin to persuade Maven
to do things it isn't designed for.
I assume that Maven can only do things in the official documentation,
and nothing else.
If I need something that is not in that documentation,
I'll use another tool.
