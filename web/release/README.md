# Releasing

This directory will help you upload all libraries in this Git repository
to your Maven repository.

## How to use

### First-time setup

1. In `settings.xml`,
put a username and its password for uploading to your Maven repository.
1. In `release` script,
set the `releaseUrl` and `snapshotUrl`
variable to the URL of your release and snapshot repository, respectively.

### Uploading everything

Run the following command
in the directory containing the root pom.xml file:

```
release/release
```

#### Cleaning your local Maven repository

You usually don't need to do this,
but if you want to, you can do this:

```
rm -r ~/.m2/repository/com/spacetimecat
```
