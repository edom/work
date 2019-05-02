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

## Install everything to your local Maven repository

```
gradle clean install
```

On my machine with default settings,
the local Maven repository is at `~/.m2/repository`.

## Add the coordinates of the dependencies

Add the dependencies to your build.gradle.

```
dependencies {
    compile "com.spacetimecat.GROUP:ARTIFACT:VERSION"
}
```
