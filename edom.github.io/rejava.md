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
    - IntelliJ IDEA, an IDE that comes with a class file decompiler
    - [Helios decompiler](https://github.com/helios-decompiler/standalone-app): "All-in-one Java reverse engineering tool"
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
        - https://mathewjhall.wordpress.com/2011/03/31/tracing-java-method-execution-with-aspectj/
        - Task-oriented AspectJ cookbook: [4.1. Capturing a Method Call](https://www.safaribooksonline.com/library/view/aspectj-cookbook/0596006543/ch04s02.html)
        - https://www.yegor256.com/2014/06/01/aop-aspectj-java-method-logging.html
        - [Tracing method calls in Java with JDB](https://teaspoon-consulting.com/articles/tracing-java-method-calls.html)
        - 2007, article, [Five ways for tracing Java execution](http://blog.zvikico.com/2007/11/five-ways-for-t.html)
        - https://stackoverflow.com/questions/19850695/does-java-have-any-mechanism-for-a-vm-to-trace-method-calls-on-itself-without-u
            - Use Javassist?
        - https://stackoverflow.com/questions/49159666/how-to-intercept-each-method-call-within-given-method-using-spring-aop-or-aspect
        - https://www.rhyous.com/2012/05/26/aop-logging-all-method-calls-and-executions-in-java-with-aspectj/
        - Eclipse Trace.aj sample file, [github](https://github.com/eclipse/org.aspectj/blob/master/tests/bugs/messyAround/aspects/Trace.aj)
        - https://blog.csanchez.org/2005/03/27/tracing-an-application-using-aspec/
- Increase log4j verbosity
- Trading login
```
2018-07-27T15:11:09.132Z	main	void esmart.trading.engine.TradingComm.a(i, g) -> void esmart.trading.engine.TradingComm.m()
2018-07-27T15:12:06.743Z	Thread-37	void esmart.trading.engine.TradingComm.a(String, String, d) -> void esmart.trading.engine.TradingComm.m()
```
    - `esmart.trading.engine.TradingComm:a(i,g)` starts two threads:
        - The thread `o` (of type `a.a.a.b`).
        - The thread `p` (of type `a.a.a.c`).
- Inferences
    - The netty major version is 3.
    - `esmart.feed.engine.network.d:a` determines our public IP address.
    - `esmart.feed.engine.ui.qo:n` Socket.connects to somewhere.
    - Initializing netty
        - `a.a.d.e:h()` initializes netty client.
        - `a.a.d.e:a(String,String)` connects.
        - It uses LengthFieldBasedFrameDecoder
            - http://netty.io/3.6/api/org/jboss/netty/handler/codec/frame/LengthFieldBasedFrameDecoder.html
            - `var1.addLast("frameDecoder", new LengthFieldBasedFrameDecoder(2147483647, 0, 4, 0, 4, true));`
            - `INFO 2018-07-25 22:14:40,830 [New I/O  worker #1] a.a.d.e Connected to : esmartbnis.com/202.129.186.235:62229`
                - server IP addresses
                    - 202.129.186.235
                    - 180.178.108.230
        - `a.a.d.e` extends `IdleStateAwareChannelHandler`.
        - After the frame is decoded, the frame payload (without the length) is passed to the handler.
        - `a.a.d.e:a(byte[])` and `a.a.d.h:a` are supposed to be inverses of each other.
        - `a.a.d.h:a()` encrypts a serialized MsgPack array using `a.a.c.c:a(byte[])`,
        a polyalphabetic substitution cipher that is its own inverse:
        applying the cipher twice (with the same key) gives the original message.
        - Application uses callback.
        Control is inverted.
        Netty calls the application.
            - http://netty.io/3.6/api/org/jboss/netty/handler/timeout/IdleStateAwareChannelHandler.html
        - Servers (open ports) at esmartbnis.com
            - News server uses port 843.
            Input is XML.
            Output is JSON.
            - Update server uses FTP (port 21 for control).
            - Feed server uses port 62229.
                - Traffic to server is compressed and encrypted.
                - Traffic from server is compressed but not encrypted.
                - Encryption
                    - Client generates symmetric key S (in practice the String representation of a random UUID).
                    - Client constructs and encodes a STOMP frame CONNECT with login, passcode, and S.
                    - Client encrypts the bytes using server RSA 1024-bit public key hardcoded in client, in ECB mode, with PKCS#1 (v1.5) padding.
                    - Client encrypts the following frames with S using `a.a.c.c:a(byte[])`.
            - Trading server uses port 63339.
        - `a.a.d.e:channelConnected` calls `a.a.d.e:d()` which does login by constructing the following message:
```
h var1 = new h(this, "CONNECT");
var1.a("login", this.l);
var1.a("passcode", this.m);
this.a(var1);
```
            - `a.a.d.h` is message frame/format/model/struct/POJO/bean, used by both client and server.
                - `a.a.d.h:a()` serializes, returning a ChannelBuffer.
                - `a.a.d.e.a(byte[])` deserializes server response, returning an `a.a.d.h`.
            - The protocol seems to be close to [STOMP as implemented by ActiveMQ Apollo](https://activemq.apache.org/apollo/documentation/stomp-manual.html),
                - Similarity: SUBSCRIBE has a `selector` header.
                - Difference: SEND has a misspelled `replyto` header.
                Apollo has `reply-to`.
        - The protocol (as decoded by `a.a.d.e:a(byte[])`):
            - Make a STOMP frame/message.
            - Encode the STOMP command as the first element in a MsgPack array.
            - Encode the STOMP header *values* in an application-predefined order as the rest of the elements of the MsgPack array.
            Header names aren't sent over the network.
            Header order is hard-coded in the application.
            - Serialize the MsgPack array into byte array X.
            - ZIP the byte array X into Y.
            - Let the length of Y be 32-bit big-endian integer N.
            - The byte array NY is what is sent over the network.
            - Some data such as login/passcode are encrypted.
        - `a.a.c.d:a(byte[],PublicKey)` encrypts the byte array using `RSA/ECB/PKCS1Padding` cipher.
        - Java libpcap wrapper
            - [pcap4j](https://www.pcap4j.org/), [github](https://github.com/kaitoy/pcap4j)
            - jnetpcap
            - pcap4j looks best, so we pick it.
                - https://github.com/java-native-access/jna/issues/281
                    - `-Djna.nosys=true`
        - When the client receives a server response:
            - `a.a.d.e:messageReceived` gets executed.
                - `a.a.d.e:b(byte[])` reacts to server response.
                Probably changes some fields.
                    - `a.a.d.e:a(byte[])` decodes server response bytes into a STOMP message.
                    It's a custom binary encoding?
                    Are the authors overlaying STOMP on a legacy protocol?
                        - The class `a.a.c.e` does ZIP compression/decompression.
                        - Package `a.a.b` is an unknown compressor/decompressor.
                        - Field `a.a.c.e:B` is compression method.
                        1 means compression in `a.a.b` package.
                        2 means ZIP.
                        - Ubuntu 14.04: `zlib-flate` CLI from `qpdf` package, compress/decompress stdin to stdout
            - Then probably a STOMP SEND/MESSAGE frame encapsulates a FIX (Financial Information Exchange) message.
        - Then `a.a.d.e:a(a.a.d.h)` sends it to the server.
    - `a.a.c.a` seems to be a base64 decoder copied from the Internet.
        - 2003-07-22: http://www.cs.huji.ac.il/~dbi/ex2/Base64Coder.java
            - https://github.com/chdh
                - http://www.source-code.biz/base64coder/java/
        - 2012-05-24: [SO 469695 answer 10736154](https://stackoverflow.com/questions/469695/decode-base64-data-in-java/10736154#10736154).
        - 2013-11-09: https://github.com/mixpanel/mixpanel-android/blob/master/src/main/java/com/mixpanel/android/util/Base64Coder.java
            - copyright year goes back to 2003
    - Schedule problems
        - The server is down every 00:00 -- 01:00 UTC+7.
        - The traffic is only interesting in the [trading hours](http://www.idx.co.id/en-us/investor/trading-hours/).
            - We can't just reverse-engineer the program at any convenient time.
                - Capture the traffic.
                - Analyze the traffic later when there is time.
    - The server is also accessible via [http://esmartbnis.com](http://esmartbnis.com).
    It uses HTTP, JavaScript, WebSocket.
    It should use the same protocol as the Java client.
    - Some strings to find?
        - `loading news failed, your connection / server not ready`
        - The string `63339` is in `TradingComm`.
    - `esmart.feed.engine.network.d` gets the client IP address.
    It's copied from the Internet.
        - created on 2013-12-06 https://stackoverflow.com/questions/9481865/getting-the-ip-address-of-the-current-machine-using-java/20418809#20418809
            - created on 2008-03-04 https://issues.apache.org/jira/browse/JCS-40
- How do we write our own ClassLoader using Javassist, and use it as the boot classloader?
- The use cases we are interested in
    - Check application updates.
    - Check news.
    - Log in.
    - Get stock data?
    - Place order?
- Producing static call graph Using `java-callgraph`
    - In hindsight, this step is unnecessary.
    We should have begun with runtime method tracing using AspectJ and bytecode decompiling using IntelliJ IDEA.
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
