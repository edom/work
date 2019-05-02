# Batching

## Use cases

### Batching by arrival time

Your front-end receives 1,000 requests per second,
but you only want to make 1 database query per 100 millisecond.
You can use `TimeBatchedCallbackFunction` for this.

### Batching by equivalence relation

Your have 100 unique things in your database.
You have 10,000 queries about those things in a batch,
but most of those queries are asking about the same thing.
You don't want to send the same query twice to the database.
You can use `DeduplicatedCallbackFunction` for this.

## Usage

### Build and install everything to your repository

If you want to use your Maven local repository,
install everything to your repository using `gradle install`.

### Add build.gradle dependency

Add this to your build.gradle project `dependencies` block:

```
compile 'com.spacetimecat.concurent:batch:0.0.0-SNAPSHOT'
```
