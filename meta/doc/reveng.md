# Reverse engineering

- motivation
    - I want to reverse-engineer a Java 6 stock trading application.
    - What are some things about the application?
        - It's obfuscated.
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
- should try?
    - "All-in-one Java reverse engineering tool" https://github.com/helios-decompiler/standalone-app
- Optimization is a partial inverse of obfuscation.
- Optimizer can help deobfuscate unnecessary instructions, but not renamings.
- Statistical renaming can help deobfuscate names.
- https://en.wikipedia.org/wiki/Reverse_engineering#Reverse_engineering_of_protocols
    - automatic online learning
- Can we reverse-engineer by supercompilation or partial evaluation?
- search keywords to try
    - java bytecode optimizer
    - java deobfuscator
    - metacompilation, supercompilation, partial evaluation, program specialization, etc. https://everipedia.org/wiki/Metacompilation/
- https://www.reddit.com/r/REMath/
- https://security.stackexchange.com/questions/29866/reverse-engineering-and-java
- Transpile Java bytecode to JavaScript, and use jsnice?
- jsnice: statistical renaming, type inference and deobfuscation http://jsnice.org/
- supercompile Java programs
    - "Ongoing work on Supercompilation of Java code (or supercompilation in general)?" http://lambda-the-ultimate.org/node/2739
    - "A Java Supercompiler and Its Application to Verification of Cache-Coherence Protocols" https://link.springer.com/chapter/10.1007/978-3-642-11486-1_16
    - 2002, "Supercompiling Java Programs" http://goertzel.org/papers/SupercompilingJavaMay2002.htm
        - What are the differences between supercompilation and partial evaluation?
