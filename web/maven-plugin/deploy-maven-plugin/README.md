# Using Maven to deploy software to worker machines

## Usage

### Initialize the current working directory

Replace `GroupId`, `ArtifactId`, `Version`, and `MainClass`
with the appropriate values.

```
mvn \
    com.spacetimecat.maven.plugin:deploy-maven-plugin:0.0.0:deploy \
    -DgroupId=GroupId \
    -DartifactId=ArtifactId \
    -Dversion=Version \
    -DmainClass=MainClass
```

This overwrites these scripts: `download`, `run`, and `runStale`.

This ensures that this file exists:
`JavaArguments`.

This overwrites these files:
`pom.xml`,
`Goal`,
`GroupId`,
`ArtifactId`,
`Version`,
and `MainClass`.

That goal generates these Bash scripts
in the current working directory:

- `download`, which downloads an artifact and all of its
transitive dependencies to the local repository;
- `runStale`, which runs `MainClass` from those downloaded artifacts;
and
- `run`, which does `download` and then `runStale`.

#### Note

Do not run `mvn install` or `mvn deploy` on the generated `pom.xml`.
It's a dummy.

### Download changed artifacts

```
./download
```

This downloads the artifact `GroupId:ArtifactId:Version`
and all its transitive dependencies.

This reads the plugin coordinates and goal from the `Goal` file.

This generates the `ClassPath` file.

This uses Maven to download those artifacts.
Maven decides whether it will redownload
an artifact that already exists in the local repository.
Usually it redownloads snapshot-version artifacts.
Usually it doesn't redownload release-version artifacts.

### Run main class from downloaded artifacts

```
./run ProgramArguments
```

This runs `download` and `runStale`.

### Run without downloading

```
./runStale [Arg0 Arg1 ...]
```

This runs `MainClass`.

This expects `MainClass` to be in the `ClassPath`.

This passes the arguments `Arg0 Arg1 ...`
to the the program's `main` method.

Each line in `JavaArguments` is one argument. For example:

```
-Dproperty0=value0
-Dproperty1=value1
-Dproperty2=value2
```

The `runStale` command is a faster `run`,
but you risk outdated artifacts.
Don't use this if you have just uploaded
a newer version of your artifact.

## Differences from mvn deploy

The `mvn deploy` command runs on a build machine
to upload the artifacts to a remote repository.
Despite the name, `mvn deploy` performs a release,
not a [deployment](https://en.wikipedia.org/wiki/Software_deployment).

This plugin, on the other hand, performs such deployment.

The following picture shows the flow of artifacts in a deployment:

```
                mvn deploy                        this plugin
build machine  ------------>  remote repository  ------------->  worker machine
```

## Prerequisites

### Worker machine

The worker machine must have Java 8, Maven 3, and Bash installed.

If the worker machine uses Ubuntu,
you can install Maven 3 this way:

```
sudo apt-get install maven
```

### Building this plugin

If you want to build this plugin, you must have Java 8 and
[Maven 3.2.2](https://maven.apache.org/docs/3.2.2/release-notes.html)
or newer.
Maven versions before that will fail to build this plugin due to
[MNG-5346](https://issues.apache.org/jira/browse/MNG-5346).
