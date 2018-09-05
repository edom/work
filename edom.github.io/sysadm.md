---
title: Administering a computer
permalink: /sysadm.html
date: 2018-08-10 03:49 +0700
---

- TOC
{:toc}

- Why use swap partitions and not swap files?
    - Defragmenting swap files might have undesirable effects.
        - https://lwn.net/Articles/317787/
- ext4 defragmentation tools
    - https://askubuntu.com/questions/221079/how-to-defrag-an-ext4-filesystem
        - e2freefrag DEV, e4defrag -c FILE
- SATA 3 Gbps controller problem?
    - https://github.com/zfsonlinux/zfs/issues/4873
- Security
    - Isolating (sandboxing) an application
        - Creating a dedicated limited user for an application
            - `adduser --system`, or just use the `nobody` user?
    - Many methods of isolation
        - https://unix.stackexchange.com/questions/384117/linux-isolate-process-without-containers
        - https://www.engineyard.com/blog/linux-containers-isolation
        - https://opensourceforu.com/2016/07/many-approaches-sandboxing-linux/
- Why isn't ssh fail2ban the default?
Why is security not the default?
- 2014, "Is there any solution to make OpenVPN authentication with Google ID?", [SF 597833](https://serverfault.com/questions/597833/is-there-any-solution-to-make-openvpn-authentication-with-google-id)
- man page usability, terminal usability, reducing cognitive load
    - [tldr.sh](https://tldr.sh/): "The TLDR pages are a community effort to simplify the beloved man pages with practical examples."
- [SO 22697049: difference between Google App Engine and Google Compute Engine](https://stackoverflow.com/questions/22697049/what-is-the-difference-between-google-app-engine-and-google-compute-engine)
- A good OS (operating system) is invisible like a good design.
    - The user can't tell whether an OS is good.
    If the OS is good, everything runs smoothly.
    - But the user can tell whether an OS is bad.
    Crashes due to non-hardware problems.
    Things that don't just work.
- https://felipec.wordpress.com/2011/06/16/after-two-weeks-of-using-gnome-3-i-officially-hate-it/
    - Why does the author disapprove of GNOME 3?
- https://libcloud.apache.org/
    - "One Interface To Rule Them All: Python library for interacting with many of the popular cloud service providers using a unified API."
- Systems should be secure by default.
    - UNIX/Linux woes
        - Why is SSH fail2ban not installed by default?
        - Why does an application run as the user?
            - I think this is a design flaw.
            It's a big gaping security hole.
            It allows an application to access all your files.
            But how do we fix this without sacrificing convenience?
            - Why is sandboxing not the default?
        - How do we understand SELinux?
            - [SELinux Explained with Examples in Easy Language](https://www.computernetworkingnotes.com/rhce-study-guide/selinux-explained-with-examples-in-easy-language.html)
            - NSA made SELinux. How trustworthy is it? Does it have NSA backdoors?
                - [linux - How trustworthy is SELinux? - Information Security Stack Exchange](https://security.stackexchange.com/questions/42383/how-trustworthy-is-selinux)
                - 2017, [The NSA has tried to backdoor linux three times : linux](https://www.reddit.com/r/linux/comments/54in5s/the_nsa_has_tried_to_backdoor_linux_three_times/)
        - What is the relationship/difference between MAC (mandatory access control) and DAC (discretionary access control)?
        Are they antonyms? Complements?
            - [access control - MAC vs DAC vs RBAC - Information Security Stack Exchange](https://security.stackexchange.com/questions/63518/mac-vs-dac-vs-rbac)
    - The only real secure way to run an untrusted application is on a different machine with no network connection.
- [pv(1): monitor progress of data through pipe - Linux man page](https://linux.die.net/man/1/pv)
- [jq](https://stedolan.github.io/jq/), [WP:jq (programming language)](https://en.wikipedia.org/wiki/Jq_(programming_language))

## Probably relevant Twitters

- [nixcraft](https://twitter.com/nixcraft): some humor, some important

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
