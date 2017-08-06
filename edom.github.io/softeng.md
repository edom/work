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
- Programmers need to own up their mistakes, and call an error an error, not a bug.
A bug is something the programmer cannot control.
A cosmic ray strike flipping a bit in RAM is a bug.
A typo in the source code is not a bug.
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
- Meilir Page Jones, "What every programmer should know about object-oriented design"
- Quartz scheduler performance
    - http://airboxlab.github.io/performance/scalability/scheduler/quartz/2017/06/20/perf_tuning_quartz.html
    - https://www.ebayinc.com/stories/blogs/tech/performance-tuning-on-quartz-scheduler/
    - https://stackoverflow.com/questions/11565993/quartz-performance
