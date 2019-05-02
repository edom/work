# The goal of this project

There should be only one way to do everything.

This project aims to standardize the way of doing things.

# About this package

This package pragmatically provides:

* command line argument/option parsing
* error handling
* logging
* multitasking (concurrency and parallelism)

There is often only one sensible way of handling errors:
barge out to the nearest handler and log a helpful message.

Our definition of _error_ is something causing the program to stray from its _happy path_;
thus our errors include what others call _exceptions_.

This has been tested with stack lts-5.14.

We need to settle on one way of handling errors;
having too many ways of handling errors prevents Haskell packages
from working with each other smoothly:
much glue code would need to be written just
to translate a package's error handling style to another's;
this just adds noise.

# Standards

* Ubuntu, Linux
* wget

* bytestring, text
* containers

* optparse-applicative
* process

* wai
* snap
* digestive-functors

# Dependencies

```
libbz2-dev
```
