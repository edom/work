---
title: Making lasting software
date: 2018-04-15 00:00 +0700
permalink: /softeng.html
---

- The keys to lasting software
    - Clear ontology
    - Task-oriented documentation
        - Documentation doesn't have to be separate from code.
    - What else?
    - *Capture* the requirement.
    Translate the requirement into software, instead of making a software system that *satisfies* the requirement.
        - The program may be rewritten, but the functional requirements, the business logic and the mathematical truths rarely change.
            - In order to make software last, we must *capture* the requirements, not make a system that satisfies the requirements.
            This requires a change in mindset.
            The software is not something made to some requirements; the software is the requirements itself.
            The software is not the shirt made-to-order; the software is the order itself, from which all satisfying shirts can be generated/derived/produced.
- Readings
    - 2018, book, "The essence of software engineering", [description](https://link.springer.com/book/10.1007/978-3-319-73897-0), [pdf](https://link.springer.com/content/pdf/10.1007%2F978-3-319-73897-0.pdf)
        - chapter "Escaping Method Prison – On the Road to Real Software Engineering"
            - some history of software engineering; method wars;
            - some retrospective
            - [SEMAT (Software Engineering Method And Theory)](https://semat.org/)
    - [ICSE (International Conference on Software Engineering) most influential articles](http://www.icse-conferences.org/mostinfluential.html)
        - "ICSE is the flagship conference of [software engineering] [...]." ([Georgios Gousios](http://www.gousios.org/blog/Report-from-ICSE-2017.html))
            - 2008, article, "Debugging Reinvented: Asking and Answering Why and Why Not Questions About Program Behavior ICSE-30"
                - https://www.cs.cmu.edu/~NatProg/whyline.html
                - https://github.com/andyjko/whyline
            - 2012, slides, "Connecting Software Architecture to Implementation: The Next 10 Years", [pdf](http://www.cs.cmu.edu/~aldrich/presentations/aldrich-icse-mip-2012.pdf)
                - 2002, article, "ArchJava: Connecting Software Architecture to Implementation", [pdf](https://www.ics.uci.edu/~andre/informatics223s2009/aldrichchambersnotkin.pdf)
            - 1997, article, "Software processes are software too, revisited", [pdf](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.148.7620&rep=rep1&type=pdf)
                - 1987, article, "Software processes are software too", [pdf](http://laser.cs.umass.edu/techreports/1987-LJO.pdf)
    - 2018, slides, "What would a science of software engineering look like?", [pdf](http://herbsleb.org/web-pres/slides/crowd-chase-2018-final-dist.pdf)
    - on complexity
        - [Renaat Verbruggen: Three universal methods of reducing complexity](http://www.computing.dcu.ie/~renaat/ca2/ca214/ca214vii.html): partitioning, hierarchy, independence
            - It also applies to other things, such as organizing this wiki.
        - 2000, article, "How complex systems fail", [pdf](http://web.mit.edu/2.75/resources/random/How%20Complex%20Systems%20Fail.pdf)
    - [Scott Hanselman: "Software doesn't work. I'm shocked at how often we put up with it."](https://www.hanselman.com/blog/EverythingsBrokenAndNobodysUpset.aspx)
    - Meilir Page Jones, "What every programmer should know about object-oriented design"
    - How do you know a software is maintainable?
        - Change all the programmers.
        If the new programmers can handle it,
        it's maintainable.
        - The only way to ensure software maintainability is periodic rotation of programmers?
- What we know
    - Adding people to a late software project makes it later.
    (Fred Brooks, "No silver bullet")
        - Newcoming programmers, no matter how experienced, need time to onboard.
        They are not immediately productive.
        - Existing programmers must spend some time helping the newcoming programmers, temporarily reducing productivity even further.
- References
    - [Standard MIDI file format]({% link smf.md %})
    - These pages may be outdated:
        - [Ansible]({% link ansible.md %})
        - [Logging]({% link logging.md %})
        - [Web scraping]({% link scrape.md %})
        - [Android]({% link android.md %}). Last time I developed for Android in 2012. My knowledge is irrelevant now.
    - Things we use, tools and services
        - git, version control
            - [Git mental model]({% link git.md %})
        - git-gui
        - gitk
        - meld, three-way diff
        - vim, text editor
        - Databases
            - [PostgreSQL]({% link pgsql.md %})
        - Build tools
            - Maven
            - [Gradle]({% link gradle.md %})
        - Third party services
            - Cloud infrastructure hosting, infrastructure as a service
                - [Amazon Web Services]({% link aws.md %})
            - Management
                - [Trello]({% link trello.md %})
        - [Kubernetes]({% link kubernetes.md %})
        - [The cloud for old-school sysadmins]({% link cloud.md %})
        - Other opinions
            - .NET stack
                - http://engineering.gopangea.com/stack
                - http://engineering.gopangea.com/2015/12/10/why-dot-net.html
- [Security]({% link security.md %})
- Developing software for the Web
    - [Software]({% link software.md %})
    - [The sad state of web development](https://news.ycombinator.com/item?id=11035143) (not everyone agrees).
    - It’s time to kill the web https://blog.plan99.net/its-time-to-kill-the-web-974a9fe80c89
        - from https://twitter.com/tehjh Jann Horn's Twitter
            - At that time Jann Horn was a member of Google's Project Zero
            https://blog.plan99.net/what-should-follow-the-web-8dcbbeaccd93
    - Why not infinite scrolling?
        - Do you have an alternative to infinite scrolling?
        - Do you handle network interruptions?
        How does the user continue scrolling after network connection is restored?
        Is there an unhandled exception in your script?
- Rants; software development woes
    - Troubleshooting Dashboard: What metrics you should monitor and why?
        - HTTP 4xx and 5xx status codes and connection failures.
        - Rising maximum latency is the first sign of something going wrong or overloaded.
- When we write a program,
we are actually creating a *mathematical model* of reality,
creating an implicit ontology,
defining what exist,
making simplifying assumptions,
discarding irrelevant aspects.
- Programmers need to own up their mistakes, and call an error an error, not a bug.
A bug is something the programmer cannot control.
A cosmic ray strike flipping a bit in RAM is a bug.
A typo in the source code is not a bug.
- Coupling is deciding what you can depend on.
    - Big Company X just released this new cool stuff, but will it still be there in 5 years?
- Quartz scheduler performance
    - http://airboxlab.github.io/performance/scalability/scheduler/quartz/2017/06/20/perf_tuning_quartz.html
    - https://www.ebayinc.com/stories/blogs/tech/performance-tuning-on-quartz-scheduler/
    - https://stackoverflow.com/questions/11565993/quartz-performance
- Attitude
    - Respect existing (legacy) code.
    It may be ugly, but *it works*.
        - Is the code really ugly?
            - Are you refusing to read it because it doesn’t fit your taste?
            Indent size?
            Snake case vs camel case?
                - Is the code truly unreadable, or are you just an asshole?
                    - How hard have you tried?
- 2017-05-20
    - What helps writing maintainable software?
        - Minimize duplication? The programming language limits deduplication?
        - Facilitate change, minimize ripple due to change
        - Flexibility of a component is proportional to its risk of changing?
- Undigested
    - What is a database that can be scaled up without downtime?
    We are willing to sacrifice consistency for tracking data.
    - Are we really doing Scrum? http://www.allaboutagile.com/the-scrumbutt-test/
    - BPMN tool: camunda editor (bpmn.io).
    - The nature of enterprise application is low-sophistication high-plumbing, so we need to make plumbing easy.
    All our programs read data, do something what that data, and write data.
    The most important things are the data and the business logic.
    - How to group code? By feature? By dependency? By author?
        - https://en.wikipedia.org/wiki/Feature-driven_development#Build_by_feature
            - "Any function that is too complex to be implemented within two weeks is
            further decomposed into smaller functions until each sub-problem is small enough to be called a feature."
    - Grails vs Spring MVC vs Play Framework?
    - distributed systems
        - [Martin Fowler's First Law of Distributed Object Design](https://martinfowler.com/bliki/FirstLaw.html): "Don't distribute your objects."
        - http://www.drdobbs.com/errant-architectures/184414966
    - Eta is GHC ported to JVM.
        - 2017-01-13: Haskell (~ GHC 7.10.3) on JVM: http://eta-lang.org/docs/html/
    - How do we make code testable?
        - Minimize its dependencies.
        - Minimize the number of things that you must set up before you can run it.
        - Why?
            - If you don't satisfy all its dependencies, you can't run it.
            - If you can't run it, you can't test it.
            - If your business logic depends on the database, you'll have to set up a database to test your business logic.
    - What do you need to test?
        - Testing is proportional to risk.
            - Riskier code should be tested more.
                - "Risk comes from not knowing what you're doing." (Warren Buffett)
                    - Buffett said that for investing, but it also applies to programming.
        - If it's obvious, don't test it. (Getters, setters, and DAOs without fancy logic)
        - If you can prove it, don't test it.
    - https://blog.codinghorror.com/discipline-makes-strong-developers/
        - Good code is more because of programmer discipline than because of the framework or language.
- Emphasize maintainability, readability, understandability, changeability?
- React reinvents Windows 1.0 (1985) https://bitquabit.com/post/the-more-things-change/
- Ramble
    - Writing software?
        - Minimize build time.
        - Minimize the time from program startup to program ready.
        Otherwise you won't test the program.
        - Understand which parts of the code have more risk.
        Risk is caused by something you don't understand. (Warren Buffett)
        - Minimize the way things can go wrong.
        If you make a variable immutable,
        there are less ways it can go wrong. (?)
        - Explicit is better than implicit?
        Prefer writing boilerplates to magical reflection stuff.
        Let the compiler help you.
        Let compilation error guide refactoring.
        - Minimize duplication?
        Minimize duplication of constants, literals, fragments?
        - Data is more important than code?
        The shape of the data is important?
        - Make every part understandable in isolation?
        - Avoid nulls? If you must use null, document it.
- 2018-05-30
    - Enterprise application stack?
        - Java 8
        - Jetty 9.4 (Servlet API 3.1.0)
        - JPA API 1.0.2 (annotations only)
        - J2HTML
        - PostgreSQL 9.5
        - J2HTML-like for C#
            - https://github.com/HtmlTags/htmltags
        - Java: Install OpenJDK 8, install IntelliJ IDEA, you're all set.
            - IntelliJ IDEA comes with Maven.
        - .NET: Install Mono, install Monodevelop 5.
            - Monodevelop 5 comes with Nuget.
        - Database should be a library, not a stand-alone server.
- software legality?
    - Harmony CLA (Contributor License Agreement)?
- What cause software errors?
    - Programming error: what the programmers think are not what they write.
    Difference between their thought and the computer's actual rules.
    Silent wrong assumption.
    - Contributing factors: bad languages, bad tools.
        - Bad languages make writing correct programs difficult.
    - Errors outside programmer control: cosmic rays, hardware problems
- What is software engineering?
    - Engineering is the application of science.
    - Civil engineering is the application of natural science.
    - Software engineering is the application of computer science.
    - What is science?
        - Science is the application of the scientific method.
        - Science is a mixture of philosophy, mathematics, and experiments.
    - What is software?
        - chapter, "What is software?: The role of empirical methods in answering the question", [description](https://link.springer.com/chapter/10.1007/978-3-319-73897-0_4), [pdf](https://link.springer.com/content/pdf/10.1007%2F978-3-319-73897-0_4.pdf)
            - "Legislation is (like?) software development."
            - "[Cooking] Recipes are software."
        - The ideal software is easy to change but doesn't change.
        The ideal software captures the essence of the problem.
        The essence of a problem is mathematical definitions.
        Mathematical definitions aren't supposed to change.
        - Software is a model of reality.
        - Software is law?
        Law is software?
            - Similarity between software and law
                - Bad software and bad law both result in unnecessarily slow system.
                - Both are based on logic.
                - Both have an ontology.
                - The law of a country is a big (possibly inconsistent) logic program.
                    - The law in writing vs the law in practice
                        - I think it is too much for anyone to know all laws that are in effect in a country.
            - Difference between software and law
                - A judge cares about both the letter of the law and the spirit of the law.
                - A computer cares only about the letter of the software.
                There is no such thing as the spirit of the software.
                    - A computer does what we write, not what we mean.
                        - The programmer follows the computer's rules. Not the other way around.
                - [WP:Letter and spirit of the law](https://en.wikipedia.org/wiki/Letter_and_spirit_of_the_law)
        - Software is executable theory of nature.
        - Software is like physics but executable.
        - https://queue.acm.org/detail.cfm?id=2693160
        - https://www.cs.umn.edu/research/research_areas/software-engineering-and-programming-languages
            - "Software is a solution to a computational problem using a formal programming language."
- How can we read SWEBOK?
Isn't it too long?
    - https://en.wikipedia.org/wiki/Software_Engineering_Body_of_Knowledge
    - http://www.sebokwiki.org/wiki/An_Overview_of_the_SWEBOK_Guide
- 2010, article, "We show how symbolic execution and Satisfiability Modulo Theories (SMT) solvers can be gainfully employed to greatly automate software debugging of evolving programs.", [downloadable as pdf](https://www.sciencedirect.com/science/article/pii/S1571066110001246)
    - confusing title: "Debugging as a Science, that too, when your Program is Changing"
- Distilling the best practices and standardizing the tools and processes
    - API (application programming interface) information systems, REST clients, REST API debuggers
        - https://insomnia.rest/
            - source code https://github.com/getinsomnia/insomnia
        - https://www.getpostman.com/
            - not open source https://stackoverflow.com/questions/43380313/postman-main-source-code-repository
    - JOOQ
    - https://zachholman.com/posts/deploying-software
    - IDE (integrated development environment)
        - How do we make an IDE?
        Can we generate an IDE from grammar?
    - How do we put academic research into practice?
        - 1995, article, "Deriving specifications from requirements: an example", [paywall](https://dl.acm.org/citation.cfm?id=225016)
        - 1998, article, "Architecture-based runtime software evolution", [paywall](https://dl.acm.org/citation.cfm?id=302181)
        - 2000, article, "Bandera: extracting finite-state models from Java source code", [paywall](https://dl.acm.org/citation.cfm?id=337234)
            - Is this related with C#'s async-await?
    - metaprogramming
        - "Spoon is a library to analyze, transform, rewrite, transpile Java source code (incl Java 9 & Java 10)." [source](https://github.com/INRIA/spoon)
- unread
    - long text: [Top-down vs. Bottom-up Hierarchy: Or, How to Design a Self-Managed Organization](http://organizationalphysics.com/2016/10/13/top-down-vs-bottom-up-hierarchy-or-how-to-build-a-self-managed-organization/)
