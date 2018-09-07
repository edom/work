---
title: Programming Bash
permalink: /bash.html
date: 2018-09-05 00:00 +0700
---

- TOC
{:toc}

## Articles

- Bash has associative arrays
    - https://www.artificialworlds.net/blog/2012/10/17/bash-associative-array-examples/
- [SierraSoftworks/bash-cli: A command line framework built using nothing but Bash and compatible with anything](https://github.com/SierraSoftworks/bash-cli)

## Error handling

I set these bash options in my script to make it fail fast:

```bash
set -o errexit
set -o nounset
set -o pipefail
```

## Bash pitfalls

This is bash version `GNU bash, version 4.3.11(1)-release (x86_64-pc-linux-gnu)` that comes with Ubuntu 14.04.

### Local variable definition ignores command substitution result

At first this seems like an unexpected interaction between function, `local` variable, `set -e (set -o errexit)`, and command substitution `$(cmd)`.

The word `local` is a shell command that has an exit status, not a keyword like `var` in JavaScript.
Bash is behaving as documented.
See the documentation for `local` in `man bash`.

```bash
fun_0() {
    local var
    var=$(false)
    echo fun_0
}

fun_1() {
    local var=$(false)
    echo fun_1
}

fun_2() {
    local var=$1
    echo fun_2
}

echo $(set -o errexit; fun_0) # Expected: This doesn't print fun_0.
echo $(set -o errexit; fun_1) # PITFALL: This prints fun_1 !!!
echo $(set -o nounset; fun_2) # Pitfall: This doesn't print fun_2, and aborts with "bash: $1: unbound variable".

```
