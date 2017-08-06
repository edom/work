---
title: Work
permalink: /work.html
date: 2018-05-15 02:31:00 +0700
---

- [Amazon Web Services]({% link aws.md %})
- [PostgreSQL]({% link pgsql.md %})
- [Security]({% link security.md %})
- [Gradle]({% link gradle.md %})
- [Trello]({% link trello.md %})
- [Class-based programming]({% link cbp.md %})
- [Software engineering management]({% link engman.md %})
- Tools
    - git, version control
        - [Git mental model]({% link git.md %})
    - git-gui
    - gitk
    - meld, three-way diff
    - vim, text editor
- These pages may be outdated:
    - [Ansible]({% link ansible.md %})
    - [Logging]({% link logging.md %})
    - [Web scraping]({% link scrape.md %})
    - [Android]({% link android.md %}). Last time I developed for Android in 2012. My knowledge is irrelevant now.
- Other opinions
    - .NET stack
        - http://engineering.gopangea.com/stack
        - http://engineering.gopangea.com/2015/12/10/why-dot-net.html
- Rants; software development woes
    - ShadowJar doesn't work with Gradle 2.13.
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
