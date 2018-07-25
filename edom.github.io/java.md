---
title: Using Java
date: 2018-07-25 15:24 +0700
permalink: /java.html
---

- How to profile a Java application startup?
    - How to make a Java application wait for a debugger to attach on startup?
- Speeding up Java startup
    - Hypothesis: IntelliJ IDEA startup is slow because it decompresses JAR.
    Java startup would be faster if the JARs were decompressed (created using `jar c0`).
        - How do we test this?
        Profile IntelliJ IDEA startup.
    - Hypothesis: The IDE would be faster if it's compiled ahead-of-time.
        - Can we cache the just-in-time compilation result?
    - Does supercompiling the IDE affect speed?
