---
title: Software engineering
date: 2018-04-15 00:00 +0700
permalink: /softeng.html
---

- References
    - [Standard MIDI file format]({% link smf.md %})
    - [Software engineer salary]({% link salary.md %})
- Sections
    - [Security]({% link security.md %})
    - [Software engineering management]({% link engman.md %})
    - [Class-based programming]({% link cbp.md %})
    - [Software]({% link software.md %})
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
- Rants; software development woes
    - Troubleshooting Dashboard: What metrics you should monitor and why?
        - HTTP 4xx and 5xx status codes and connection failures.
        - Rising maximum latency is the first sign of something going wrong or overloaded.
    - [The sad state of web development](https://news.ycombinator.com/item?id=11035143) (not everyone agrees).
    - long text: [Top-down vs. Bottom-up Hierarchy: Or, How to Design a Self-Managed Organization](http://organizationalphysics.com/2016/10/13/top-down-vs-bottom-up-hierarchy-or-how-to-build-a-self-managed-organization/)
    - Why not infinite scrolling?
        - Do you have an alternative to infinite scrolling?
        - Do you handle network interruptions?
        How does the user continue scrolling after network connection is restored?
        Is there an unhandled exception in your script?
- When we write a program,
we are actually creating a *mathematical model* of reality,
creating an implicit ontology,
defining what exist,
making simplifying assumptions,
discarding irrelevant aspects.
- [Scott Hanselman: "Software doesn't work. I'm shocked at how often we put up with it."](https://www.hanselman.com/blog/EverythingsBrokenAndNobodysUpset.aspx)
- [Renaat Verbruggen: Three universal methods of reducing complexity](http://www.computing.dcu.ie/~renaat/ca2/ca214/ca214vii.html): partitioning, hierarchy, independence
    - It also applies to other things, such as organizing this wiki.
- Programmers need to own up their mistakes, and call an error an error, not a bug.
A bug is something the programmer cannot control.
A cosmic ray strike flipping a bit in RAM is a bug.
A typo in the source code is not a bug.
- Coupling is deciding what you can depend on.
    - Big Company X just released this new cool stuff, but will it still be there in 5 years?
- Meilir Page Jones, "What every programmer should know about object-oriented design"
- Quartz scheduler performance
    - http://airboxlab.github.io/performance/scalability/scheduler/quartz/2017/06/20/perf_tuning_quartz.html
    - https://www.ebayinc.com/stories/blogs/tech/performance-tuning-on-quartz-scheduler/
    - https://stackoverflow.com/questions/11565993/quartz-performance
- How do you know a software is maintainable?
    - Change all the programmers.
    If the new programmers can handle it,
    it's maintainable.
    - The only way to ensure software maintainability is periodic rotation of programmers?
- Attitude
    - Respect existing code.
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
    - [PMSE 2780: How to organize knowledge within a wiki?](https://pm.stackexchange.com/questions/2780/how-to-organize-knowledge-within-a-wiki)
    - Are we really doing Scrum? http://www.allaboutagile.com/the-scrumbutt-test/
    - BPMN tool: camunda editor (bpmn.io).
    - https://www.quora.com/What-is-the-best-way-to-organize-a-company-wiki
    - The nature of enterprise application is low-sophistication high-plumbing,
    so we need to make plumbing easy.
    All our programs read data, do something what that data, and write data.
    The most important things are the data and the business logic.
    - How to group code? By feature? By dependency? By author?
        - https://en.wikipedia.org/wiki/Feature-driven_development#Build_by_feature
            - "Any function that is too complex to be implemented within two weeks is
            further decomposed into smaller functions until each sub-problem is small enough to be called a feature."
    - Grails vs Spring MVC vs Play Framework?
    - Why are Android phones so sluggish?
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
- Reverse engineering
    - https://reverseengineering.stackexchange.com/questions/1817/is-there-any-disassembler-to-rival-ida-pro
    - http://www.capstone-engine.org/
- It’s time to kill the web https://blog.plan99.net/its-time-to-kill-the-web-974a9fe80c89
    - from https://twitter.com/tehjh Jann Horn's Twitter
        - At that time Jann Horn was a member of Google's Project Zero
        https://blog.plan99.net/what-should-follow-the-web-8dcbbeaccd93
- React reinvents Windows 1.0 (1985) https://bitquabit.com/post/the-more-things-change/
- Why is Jekyll slow on my machine?
    - It's fast on GitHub.
    - What is making it slow?
        - How do we find that out?
            - Is there a stack-sampling profiler for Ruby?
                - Yes, `conscho` in [SO 4092641](https://stackoverflow.com/questions/4092641/profiling-ruby-code) recommends `stackprof`.
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
- software legality?
    - Harmony CLA (Contributor License Agreement)?
- What cause software errors?
    - Programming error: what the programmers think are not what they write.
    Difference between their thought and the computer's actual rules.
    Silent wrong assumption.
    - Contributing factors: bad languages, bad tools.
        - Bad languages make writing correct programs difficult.
    - Errors outside programmer control: cosmic rays, hardware problems
