This package pragmatically provides:

* error handling
* logging
* multitasking (concurrency and parallelism)

There is often only one sensible way of handling errors:
barge out to the nearest handler and log a helpful message.

Our definition of _error_ is something causing the program to stray from its _happy path_;
thus our errors include what others call _exceptions_.

This has been tested with stack lts-5.14.
