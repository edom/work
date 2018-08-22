---
title: Reverse software engineering
date: 2018-07-21 23:57 +0700
permalink: /reveng.html
---

- Overview
    - [WP:Reverse engineering](https://en.wikipedia.org/wiki/Reverse_engineering)
        - Knowledge Discovery Metamodel
    - 2006, article, "An Overview of the State-of-The-Art Reverse Engineering Techniques", [pdf](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.107.1164&rep=rep1&type=pdf)
- In reverse engineering, tools are very important.
- jsnice: statistical renaming, type inference and deobfuscation http://jsnice.org/
- Optimization is a partial inverse of obfuscation.
- Optimizer can help deobfuscate unnecessary instructions, but not renamings.
- Statistical renaming can help deobfuscate names.
- https://en.wikipedia.org/wiki/Reverse_engineering#Reverse_engineering_of_protocols
    - automatic online learning
- 2003, PhD thesis, "Object-Oriented Reverse Engineering: Coarse-grained, Fine-grained, and Evolutionary Software Visualization",
[pdf](https://pdfs.semanticscholar.org/73d2/6f1c550ea59352252adf288d9314d4d98ade.pdf),
[backup pdf](http://soft.vub.ac.be/FFSE/Publications/LanzaPhD2003.pdf)
- 2002, Diplomarbeit, "Navigation in Object-Oriented Reverse Engineering", [pdf](http://www.inf.usi.ch/faculty/lanza/Downloads/Schw02a.pdf)
- Ambiguous title: The phrase "Reverse-engineering software" can mean two things:
    - software that helps people do reverse-engineering
    - the act of reverse-engineering a computer program (that is, software)
- Reverse-engineering and deobfuscation
    - Optimization is a partial inverse of obfuscation.
    - An optimizer can be used to deobfuscate.
    - Let's make a JVM bytecode optimizer in Haskell?
    - Stolas / Reverse Engineering Toolkit https://gist.github.com/Stolas/173b174a1d62734540c360d8f66850d9
    - https://resources.infosecinstitute.com/top-8-reverse-engineering-tools-cyber-security-professionals/#gref
    - https://www.quora.com/Whats-your-best-reverse-engineering-tools-and-why
- Where does this belong: programming language research, statistics, or reverse engineering?
    - JSNice: a statistical approach to program deobfuscation
        - http://www.jsnice.org/
        - http://www.nice2predict.org/
        - https://www.sri.inf.ethz.ch/jsnice.php
