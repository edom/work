# A Prolog interpreter (in progress)

This is not yet working.

This is planned to be a Prolog interpreter written in C++ and Prolog.

## Design goals

We have these dreams:

- maintainability, simplicity, understandability, longevity, elegance, minimalism, correctness
- seamless interoperation of programming languages, especially C, C++, and Prolog
- write low-level code in Prolog

## Build

```
cd ../build
g++ -Wall -g ../prolog/prolog.cpp -o prolog
```
