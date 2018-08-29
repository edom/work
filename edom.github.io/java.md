---
title: Using Java
date: 2018-05-16 03:03 +0700
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
- We can use the JVM without the Java language.
    - https://en.wikipedia.org/wiki/List_of_JVM_languages
- Profiling
    - Install NetBeans.
    - Choose 'Profile' in the menu, and then 'Attach to External Process'.
    - Click the down-pointing triangle on the right of the Attach button.
    - Choose 'Setup Attach to Process...'.
    - Select 'Manually started remote Java process'.
    - Choose the remote operating system.
    - Follow further instructions in NetBeans.
        - If you need `JAVA_HOME` on Oracle JRE 8 on Ubuntu 14.04, use `/usr/lib/jvm/java-8-oracle/jre`.
- How do I start the JVM with a profiling agent?
- Let the compiler help you.
    - If you make your the fields of your Java class final, you will never forget to set it.
        - You don't need to remember anything.
        - It just won't compile.
    - You can have dependency injection without dependency injection container/framework.
        - https://sites.google.com/site/unclebobconsultingllc/blogs-by-robert-martin/dependency-injection-inversion
        - If you have so many classes that instantiating them hurts, don't create so many classes in the first place.
        - Neutral article http://fabien.potencier.org/do-you-need-a-dependency-injection-container.html
        - Very opinionated article, borderline fanatical http://www.yegor256.com/2014/10/03/di-containers-are-evil.html
        - http://blog.ploeh.dk/2010/02/03/ServiceLocatorisanAnti-Pattern/
            - "In short, the problem with Service Locator is that it hides a class' dependencies, causing run-time errors instead of compile-time errors [...]"
- Package by feature, not by layer: http://www.javapractices.com/topic/TopicAction.do?Id=205
    - Dont separate model, data, entity, accessor, and service packages; package by feature not layer
- IntelliJ IDEA can open a Maven project whose POM XML file name is not pom.xml.
- JVM memory usage problem
    - Tuning JVM memory usage
        - https://docs.oracle.com/cd/E13150_01/jrockit_jvm/jrockit/geninfo/diagnos/tune_footprint.html
        - https://www.javacodegeeks.com/2017/11/minimize-java-memory-usage-right-garbage-collector.html
    - "Make JVM respect CPU and RAM limits" https://hub.docker.com/_/openjdk/
    - https://blogs.oracle.com/java-platform-group/java-se-support-for-docker-cpu-and-memory-limits
- 2017-02-21
    - https://github.com/javaparser/javaparser
    - https://github.com/javaparser/javasymbolsolver
- https://github.com/java-deobfuscator/deobfuscator
- 2017-05-20
    - Generating Java code
        - Alternatives
            - Use Python to generate Java code?
            Python comes installed with Ubuntu.
            - Use Java CodeModel to generate Java code.
            https://github.com/javaee/jaxb-codemodel
            - Use the Haskell package `language-java`
            to generate Java code.
            - Read table metadata from DataSource,
            generate Java source file for Entity and DAO.
- 2017-05-18
    - Java is procedural.
    - `object.method(argument)` is a syntactic sugar for `method(object, argument)`.
    - Where should the method `m` be defined?
    It can be defined in both `a` and `b`.
    `a.m(b)` or `b.m(a)` or `C.m(a,b)`?
    If you have to ask this, your design is wrong.
    - Antipattern: two classes A and B with conversion from A to B and B to A?
        - Solution: Delete one of them?
- http://blog.sokolenko.me/2014/11/javavm-options-production.html
- [MaintainJ: "We simplify the complexity of maintaining Java code"](http://maintainj.com/index.html)
    - Watch the demo.
    - What does it do, as seen by programmers, in non-marketing tech-speak?
        - Start/stop dumping call trace of a running JVM into a file.
        - Open the dump as a sequence diagram in Eclipse.
        - Some filtering.
- [Jconsole to remote servers, easily \| Jethro Carr](https://www.jethrocarr.com/2013/11/30/jconsole-to-remote-servers-easily/)
