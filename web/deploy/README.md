# Deploying

This directory will help you upload all libraries in this Git repository
to your Maven repository.

## How to use

### First-time setup

1. In `settings.xml`,
put a username and its password for uploading to your Maven repository.
1. In `deploy` script,
set the `url` variable to the URL of your repository.

### Uploading everything

Run the following command
in the directory containing this readme:

```
./deploy
```

#### Cleaning your local Maven repository

You usually don't need to do this,
but if you want to, you can do this:

```
rm -r ~/.m2/repository/com/spacetimecat
```
