---
title: Gradle
permalink: /gradle.html
date: 2018-05-15 23:10:00 +0700
---

- Conclusion:
    - I still haven't found any reason to switch from Maven to Gradle
    (other than "because this project is already using it").
- Which version of Gradle are we talking about?
    - Gradle 2.9.
- What problems does Gradle solve?
    - Dependency management (picking the libraries' versions and downloading the corresponding JAR files) for Java.
- Do we have those problems?
    - Yes. Software has external dependencies.
- Does Gradle 2.9 solve those problems well?
    - No. Gradle 2.9's dependency resolution algorithm doesn't compute the intersection of version ranges. (Maybe now it does.)
        - Why does it have this defect? Maybe Gradle developers had different priorities, or they didn't know how to do it.
        - [Danilo Pianini: Version ranges resolution in Gradle is insane](https://danysk.github.io/information%20technology/gradle-dependency-resolution-is-insane/)
    - However, there are times we want exact versions instead of version ranges. You want deterministic builds.
    In this case, there's no need to compute intersections.
        - [Dan Lew: Don't use dynamic versions for your dependencies](http://blog.danlew.net/2015/09/09/dont-use-dynamic-versions-for-your-dependencies/)
    - On the other hand, we want to benefit from library updates. Maybe there are security fixes.
    So we want version ranges?
        - But this assumes that the library maintainer obeys Semantic Versioning.
- Why are we using Gradle instead of Maven? What Maven annoyances does Gradle hide from us?
    - Gradle build scripts are shorter (but IDEA autocompletes Maven pom.xml).
- Why *not* Gradle?
    - Why Maven instead of Gradle?
        - IDEA integrates better with Maven because pom.xml is configuration, not program.
        (But IDEA can also open build.gradle?)
            - Opening a pom.xml just works in IDEA.
- How does Gradle 2.9 annoy us?
    - We often have to explicitly tell Gradle 2.9 the exact versions of the libraries we want because it doesn't compute intersections properly.
    Maven computes intersections.
    - Gradle doesn't download dependencies in parallel. (But neither does Maven.)
    - Gradle 2.9 doesn't generate a useful Maven POM, only a minimally valid POM.
- When should I split a Gradle subproject or a Maven module?
    - When we need to reuse one subproject without the others.
    - If they don't make sense separately, don't split them; it'll just slow down the build for nothing.
    - The same goes for Maven modules.
