---
title: Reverse-engineering the network protocol used by a Java desktop application
date: 2018-07-24 00:10 +0700
permalink: /rejava.html
---

- I want to reverse-engineer a Java 6 stock trading application.
    - The app's license agreement doesn't mention anything about reverse engineering.
    - I only want the network protocol.
    I don't need to reverse the GUI.
- Tools
    - The tool [java-callgraph](https://github.com/gousiosg/java-callgraph) is mentioned often,
    such as in [SO 29382231](https://stackoverflow.com/questions/29382231/how-do-i-trace-methods-calls-in-java)
    and [SO 4951517](https://stackoverflow.com/questions/4951517/static-analysis-of-java-call-graph).
    - AspectJ Aspect Oriented Programming, bytecode instrumentation, bytecode manipulation
    - [Helios decompiler](https://github.com/helios-decompiler/standalone-app): "All-in-one Java reverse engineering tool"
- Producing static call graph Using `java-callgraph`
    - Building, assuming JDK 8 and Maven 3 are installed:
```
git clone git@github.com:gousiosg/java-callgraph.git
cd java-callgraph
mvn package
```
    - Producing static call graph of classes in JAR files:
```
java -jar target/javacg-0.1-SNAPSHOT-static.jar JAR1 JAR2 ...
```
        - The graph representation is adjacency list.
        One line of text represents one edge in the call graph.
        The format is documented at its [GitHub page](https://github.com/gousiosg/java-callgraph).
        A line `X Y` means the method X calls the method Y.
        - It only works with JARs.
        It doesn't work with directories.
    - We can also produce a class dependency graph using the `jdeps` tool that comes with the JDK.
    - Discerning the static call graph
        - `egrep -i 'java.net|netty|crypt|msgpack|buffer' static | egrep -v 'java.lang|java.awt|javax.swing' | sort | less`
        - Which methods have anything to do with encryption?
            - `awk '$2 ~ /crypt/' static | sort`
            - The class `a.a.c.d` is the only class that uses `javax.crypto.*`.
        - Which methods use the class `a.a.c.d`?
            - `awk '$2 ~ /a.a.c.d/' static | sort`
        - What other methods are interesting?
            - `java.net.Socket.get(Input|Output)Stream`
            - `netty`
        - After reading the static call graph, I think this might be how the app sends a message:
            - Construct plaintext message using msgpack.
            - Encrypt the plaintext, producing ciphertext, probably a ByteBuffer.
            - Send the ciphertext with java.net or netty.
- Tracing runtime method calls using AspectJ
    - Download the latest stable release of AspectJ (1.9.1 as of 2018-04-20) from the [official download page](https://www.eclipse.org/aspectj/downloads.php).
    It's an installer.
        - Install it using `java -jar aspectj-1.9.1.jar` (change the filename if required).
            - Read the `README-AspectJ.html` whose exact location is displayed at the end of installation.
        - Read [The AspectJ™ development environment guide, Chapter 5, "Load-time-weaving"](https://www.eclipse.org/aspectj/doc/released/devguide/ltw.html).
    - The plan
        - Print something everytime a method is called.
        - Insert bytecode at the beginning of every method.
        - Exclude `java.*` and `javax.*` classes, but include `java.net.*` and `java.nio.*`.
        Also include everything else.
    - What is the minimum knowledge necessary to make sense of AspectJ?
    Where is the crash course?
        - What can it do that we need?
            - Using AspectJ, we can insert tracing code at the beginning of every method at class load time.
        - "Aspect weaving" is bytecode manipulation.
        - short read, [baeldung.com: Intro to AspectJ](http://www.baeldung.com/aspectj)
        - long read, [The AspectJ™ Programming Guide](https://www.eclipse.org/aspectj/doc/next/progguide/index.html)
        - [Downloads](https://www.eclipse.org/aspectj/downloads.php): Do we have to do this? Can we use Maven instead?
        - [FAQ](https://www.eclipse.org/aspectj/doc/released/faq.php)
    - Sample codes
        - Good task-oriented AspectJ cookbook: [4.1. Capturing a Method Call](https://www.safaribooksonline.com/library/view/aspectj-cookbook/0596006543/ch04s02.html)
        - https://www.yegor256.com/2014/06/01/aop-aspectj-java-method-logging.html
        - [Tracing method calls in Java with JDB](https://teaspoon-consulting.com/articles/tracing-java-method-calls.html)
        - 2007, article, [Five ways for tracing Java execution](http://blog.zvikico.com/2007/11/five-ways-for-t.html)
        - https://stackoverflow.com/questions/19850695/does-java-have-any-mechanism-for-a-vm-to-trace-method-calls-on-itself-without-u
            - Use Javassist?
        - https://stackoverflow.com/questions/49159666/how-to-intercept-each-method-call-within-given-method-using-spring-aop-or-aspect
        - https://mathewjhall.wordpress.com/2011/03/31/tracing-java-method-execution-with-aspectj/
        - https://www.rhyous.com/2012/05/26/aop-logging-all-method-calls-and-executions-in-java-with-aspectj/
- The use cases we are interested in
    - Check application updates.
    - Check news.
    - Log in.
    - Get stock data?
    - Place order?
- The plan:
    - Do we need a call graph or call trace?
    Will a trace suffice?
        - A trace is a line that consists of time, thread id, qualified class name, and method signature.
        - Use AOP (aspect-oriented programming) that uses bytecode manipulation such as AspectJ.
    - Compute static call graph.
        - Find patterns in the call graph.
        Find calls from non-network class to network class.
            - A *network class* is a class whose qualified name matches any of these patterns:
                - `java.net.*`
                - `java.nio.*`
                - `*.netty.*`
        - Don't exclude any class.
        Filter it later instead.
    - https://stackoverflow.com/questions/29451704/using-javassist-to-log-method-calls-and-argument-values-how-to-make-a-logger-cl
    - AspectJ load-time weaving (LTW)
        - https://stackoverflow.com/questions/13781372/java-aspect-oriented-programming-runtime-aspect-weaving-and-class-loading-time
        - even more intrusive
            - 2008, article, "Aspect Weaving in Standard Java Class Libraries", [pdf](http://www.inf.usi.ch/faculty/binder/documents/pppj08.pdf)
    - Find tools.
        - 2008, article, "The Rigi Reverse Engineering Environment", [pdf](https://www.rose-hulman.edu/class/cs/csse575/Resources/rigi-wasdett2008-paper06.pdf)
            - https://en.wikipedia.org/wiki/Rigi_(software)
            - requires source code
        - 2001, article, "Shimba—An environment for reverse engineering Java software systems", [abstract](https://www.researchgate.net/publication/220280416_Shimba-An_environment_for_reverse_engineering_Java_software_systems)
            - The abstract sounds promising, but where is the paper?
            Where is the source code?
            It's not on GitHub.
            That article is locked up somewhere and the source code is nowhere in sight.
            That research practically doesn't exist; it practically never happened.
            What a waste.
        - 2018, article, "Scientists should be solving problems, not struggling to access journals", [html](https://www.theguardian.com/higher-education-network/2018/may/21/scientists-access-journals-researcher-article)
        - 2002, article, "Dynamic Analysis For Reverse Engineering and Program Understanding", [pdf](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.1091&rep=rep1&type=pdf)
        - Generating Java dynamic call graph - no license, course assignment https://github.com/EmilyShepherd/Dynamic-Call-Graph
            - https://github.com/dkarv/jdcallgraph
            - https://stackoverflow.com/questions/6540423/simple-dynamic-call-graphs-in-java
        - https://erik.doernenburg.com/2008/09/call-graph-visualisation-with-aspectj-and-dot/
    - Find some information about how to debug Java bytecode.
        - 2018-07-22 decision: Write a Java program that uses the JDI (Java Debug Interface).
            - 2018-07-23 hindsight: That was a bad decision.
            We should have computed the call graph first.
        - References we will often use
            - [Java 8 JDI javadoc](https://docs.oracle.com/javase/8/docs/jdk/api/jpda/jdi/index.html)
        - Details:
            - Things that come with JDK 7:
                - [JPDA (Java Platform Debug Architecture)](https://docs.oracle.com/javase/7/docs/technotes/guides/jpda/index.html)
                    - [JPDA examples that come with JDK](https://docs.oracle.com/javase/7/docs/technotes/guides/jpda/examples.html)
                        - [javadt](https://docs.oracle.com/javase/7/docs/technotes/guides/jpda/javadt.html) GUI tool
                        - [jdb](https://docs.oracle.com/javase/7/docs/technotes/tools/windows/jdb.html) command-line tool
                        - [trace](https://docs.oracle.com/javase/7/docs/technotes/guides/jpda/trace.html)
                    - JPDA = JVMTI + JDWP + JDI
                        - JVMTI = Java Virtual Machine Tool Interface
                        - JDWP = Java Debug Wire Protocol
                        - JDI = Java Debug Interface
                            - "We recommend the JDI layer for all debugger development." ([source](https://docs.oracle.com/javase/7/docs/technotes/guides/jpda/architecture.html#jdi))
                            - This enables us to write programs that debug programs.
            - Sample code
                - There doesn't seem to be any official tutorials.
                - [dzone.com: Generating a minable event stream with JDI](https://dzone.com/articles/generating-minable-event)
                - [Java: Using JPDA to write a debugger](http://illegalargumentexception.blogspot.com/2009/03/java-using-jpda-to-write-debugger.html)
                - [JDK 7 jdb source code](https://github.com/openjdk-mirror/jdk7u-jdk/blob/master/src/share/classes/com/sun/tools/example/debug/tty/TTY.java).
    - Use JDB to debug the program.
        - Or perhaps Eclipse? [crowdstrike.com: Native Java Bytecode Debugging without Source Code](https://www.crowdstrike.com/blog/native-java-bytecode-debugging-without-source-code/)
    - Find out where to focus.
        - Map network-related call graph.
            - Put a breakpoint at Socket.getOutputStream.
            - It uses netty and thread pools, encumbering the recovery of the interesting stack frames.
            - It uses a serialization library `msgpack`. Perhaps put breakpoints there?
    - Write a program to dump the call graph of every call to netty class from non-netty class.
- Obstacles and woes:
    - Its obfuscation outsmarts IntelliJ IDEA's decompiler.
    - There is no `chdir` in Java standard library, and it seems that there will never be.
    See [WONTFIX: JDK-4045688: "Add chdir or equivalent notion of changing working directory"](https://bugs.openjdk.java.net/browse/JDK-4045688).
    - Method entry event is abysmally slow because it forces bytecode interpretation ([SO 751105](https://stackoverflow.com/questions/751105/why-does-the-debugged-program-slow-down-so-much-when-using-method-entry-debuggin/api.stackexchange.com)).
    Others experience this problem too:
    [NetBeans 47759](https://netbeans.org/bugzilla/show_bug.cgi?id=47759),
    [Eclipse 90870](https://bugs.eclipse.org/bugs/show_bug.cgi?id=90870),
    [SO 48114042](https://stackoverflow.com/questions/48114042/jpda-methodentryevent-causing-app-to-run-very-slow).
    - JDI implementation oversight
        - Breakpointing an abstract method throws a NullPointerException with no message, because `m.location()` returns `null` which can't be passed to `createBreakpointRequest` which doesn't check for nulls.
- Do I have to do this time-consuming stuff? What are the alternatives?
    - I can buy data.
    - I can ask the company to provide some documentation about their protocols.
        - Whom should I talk to?
            - I can ask my sales representative whether he knows the people who made or is maintaining the application.
            - Someone in a local Java User Group might know something.
                - https://groups.google.com/forum/#!forum/jugi
                    - The mailing list has devolved into a job board.
                - Is there a chat? IRC?
                - https://www.facebook.com/groups/ForumJavaIndonesia/
            - People who have made a similar application
                - 2008 PT Trimegah Securities Tbk https://www.linkedin.com/in/syahreza-pahlevi-ginting-0041b52/
                - 2016 BCA Sekuritas https://www.linkedin.com/in/kusnandartoni/
                - 2010 https://www.linkedin.com/in/elvino-a07277b/
                - unclear linkedin profile
                    - 2013-2016 Bareksa, 2012 Limas https://www.linkedin.com/in/arisman-26672a52/
    - I can try to convince the company to open-source this platform.
        - Somewhat unlikely in this country that is behind the US by 10 years or perhaps even more.
    - I can switch to the company's other product that is a web app that uses websockets.
        - Which one is easier to reverse-engineer?
    - These are possible, but I would rather not do these:
        - Get a job in a securities company, and then get the required information.
- The plan:
    - Try existing deobfuscators, supercompilers, disassemblers, debuggers, and other tools.
- Can we reverse-engineer by supercompilation or partial evaluation?
- search keywords to try
    - java bytecode optimizer
    - java deobfuscator
    - metacompilation, supercompilation, partial evaluation, program specialization, etc. https://everipedia.org/wiki/Metacompilation/
- https://www.reddit.com/r/REMath/
- https://security.stackexchange.com/questions/29866/reverse-engineering-and-java
- Transpile Java bytecode to JavaScript, and use jsnice?
- supercompile Java programs
    - "Ongoing work on Supercompilation of Java code (or supercompilation in general)?" http://lambda-the-ultimate.org/node/2739
    - "A Java Supercompiler and Its Application to Verification of Cache-Coherence Protocols" https://link.springer.com/chapter/10.1007/978-3-642-11486-1_16
    - 2002, "Supercompiling Java Programs" http://goertzel.org/papers/SupercompilingJavaMay2002.htm
        - What are the differences between supercompilation and partial evaluation?
- Reverse engineering Java
    - 2012, article, "A Patterns Based Reverse Engineering Approach for Java Source Code", [description](https://www.researchgate.net/publication/261391889_A_Patterns_Based_Reverse_Engineering_Approach_for_Java_Source_Code)
    - Reverse engineering Java using Eclipse
        - [wiki.eclipse.org: Java reverse engineering](https://wiki.eclipse.org/Java_reverse_engineering)
    - Java agents
        - https://blog.takipi.com/double-agent-java-vs-native-agents/
    - Related JEPs (Java Enhancement Proposals)
        - 2018-07-23: last updated in 2015, created in 2011, [JEP 159: Enhanced Class Redefinition](http://openjdk.java.net/jeps/159), [JDK-8046149](https://bugs.openjdk.java.net/browse/JDK-8046149)
