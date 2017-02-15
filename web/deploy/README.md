# Using Maven to deploy software to worker machines

The `mvn deploy` command runs on a build machine
to upload the artifacts to a remote repository.
Despite the name, `mvn deploy` performs a release,
not a [deployment](https://en.wikipedia.org/wiki/Software_deployment).

The `deploy` script runs on a worker machine.
The script can download an artifact and all of its transitive dependencies
from a remote repository to the local repository.
The script can run an artifact in the local repository.

The following picture shows the flow of artifacts in a deployment:

```
                mvn deploy                        deploy run
build machine  ------------>  remote repository  ------------>  worker machine
```

## Prerequisites

The worker machine must have Maven 3 and Bash installed.

If the worker machine uses Ubuntu,
you can install Maven 3 like this:

```
sudo apt-get install maven
```

## Usage

Replace `deploy` with the path to the `deploy` script.

We assume you are using Bash.

### download

This downloads the specified artifact from a remote repository to the local repository.

This generates a `classpath` file for `run`.

```
groupId=G artifactId=A version=V deploy download
```

### run

This is like `download` followed by `run-stale`.

```
groupId=G artifactId=A version=V deploy run <jvm-args> <main-class> <program-args>
```

### run-stale

This is a faster `run`, but you risk outdated artifacts.
Don't use this if you have just run `mvn deploy`.

This command exists for development purposes.

```
groupId=G artifactId=A version=V deploy run-stale <jvm-args> <main-class> <program-args>
```

Unlike `run`, in `run-stale` you can omit the environment variables
if the `classpath` file already exists.

```
deploy run-stale <jvm-args> <main-class> <program-args>
```

## Notes

Do not run `mvn install` or `mvn deploy`
on the `pom.xml` that comes with this script.
It's a dummy.
