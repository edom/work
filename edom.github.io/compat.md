---
title: Maintaining backward-compatibility
permalink: /compat.html
date: 2018-09-01 03:50 +0700
---

# Maintaining backward-compatibility

## The key of library-writing is empathy.

Think about your users.
Don't break their stuffs.

See [Eternal compatibility in theory](https://wiki.haskell.org/The_Monad.Reader/Issue2/EternalCompatibilityInTheory).

Making only backward-compatible changes simplifies the lives of people who depend on you.

Library authors must maintain some backward compatibility.
They can't just make arbitrary changes and break things.

- https://plan99.net/~mike/writing-shared-libraries.html

## Choosing a versioning policy

Use what everybody else is already using in your ecosystem:
[Dhall versioning policy](https://github.com/dhall-lang/dhall-lang/blob/master/VERSIONING.md),
[Haskell PVP Specification](https://pvp.haskell.org/),
[Semantic Versioning](https://semver.org/),
etc.

If everybody followed eternal compatibility, versioning policies would be irrelevant and upgrades would be smoother.

## Following Haskell package versioning policy

- [Michael Snoyman's personal take on PVP version upper bounds](https://gist.github.com/snoyberg/f6f10cdbea4b9e22d1b83e490ec59a10).
- The alternative to Cabal PVP is compile error, or, even worse, logic error and runtime failure?
    - Cabal PVP depends on library authors/maintainers to test and update their dependency bounds.

## Backward-compatibility hall of fame

These systems may be too backward-compatible.

2018-09-01: [IBM Z mainframes](https://www.ibm.com/support/knowledgecenter/en/linuxonibm/liaag/wkvm/wkvm_c_overview.htm).
The page says that the 2018 system is mostly backwards-compatible to the 1964 system.

- 2014: [Are IBM Mainframes Really Backward Compatible? - LongEx Mainframe Quarterly](http://www.longpelaexpertise.com.au/ezine/IBMBackwardCompatibility.php)

Windows 95: [Much more than you would ever know.. The original version of Sim City was writt... \| Hacker News](https://news.ycombinator.com/item?id=2281932)
