- Things far from being finished
    - [meta](meta/)
        - library for writing metaprograms in Haskell
        - use Haskell as metalanguage
    - [sound](sound/)
        - Find out whether real-time sound synthesis is possible with GHC Haskell.
        - Write some ugly code involving GHC primitives (unboxed things).
        - Hide the ugliness behind a newtype hidden in a module.
        - Assume that GHC inlines well.
    - [site](site/)
- Knowledge base
    - Haskell
        - [Cabal tragedies](cabal.md)
- 2018-07-16: I stopped using Travis CI because `cabal new-build` ran out of memory.
- The `travis` branch
    - Pushing to this branch will trigger a rebuild.
    - This branch is meant to be rebased onto `origin/master`.
    - This branch is meant to be force-pushed.
    - The file `.travis.yml` shall stay in this branch.
    - Travis CI woes
        - @cabal new-build@ runs out of memory.
            - Reduce @-j4@ to @-j1@?
            - Add @optimization: False@ into @travis/cabal.config@.
                - @cabal --config-file=travis/cabal.config@
        - Related GHC woes
            - [#9221 (super!) linear slowdown of parallel builds on 40 core machine](https://ghc.haskell.org/trac/ghc/ticket/9221)
- Some notes about @optional-packages@ in @cabal.project@:
    - Clone [Yi editor source](https://github.com/yi-editor/yi) into @yi@.
        - Hackage's yi-0.17.1 (Nov 2017) is not ready for GHC 8.
- information organization
    - An information is a sentence, a paragraph, a picture, or anything that helps us accomplish something.
        - Here "information" is a countable noun.
    - Informations should be grouped by the task it helps us accomplish.
        - Every task T requires a set of informations, which should be put together in one place.
        - If doing task T requires informations J and K, then J and K should be grouped together.
        - Duplication problem.
            - Suppose: both task T and task U requires information J.
            - Where should J be put?
    - The value of information J should be measured by the ratio V/E, where V is the value of the task that J helps us accomplish, and E is our effort for obtaining J.
    - Example of combinatorial explosion of categories.
        - Which should a red car be grouped together with: a red hat or a blue car?
            - Which should a big old man be grouped together with: a big old woman, a big young man, or a small old man?
- When can I have a Debian phone?
Why isn't there any?
    - https://www.androidpit.com/turn-your-android-device-into-a-linux-pc-without-rooting
    - https://www.thanassis.space/android.html
    - https://www.linuxjournal.com/content/maru-os-brings-debian-your-phone
    - or just buy a laptop/notebook/netbook and a GSM dongle
- What is a symposium?
    - From Greek meaning "to drink together".
        - https://en.wikipedia.org/wiki/Symposium
    - What is the difference between conference, congregation, symposium, seminar?
- ramble
    - https://www.researchgate.net/project/Ontology-oriented-programming
    - http://www.doc.ic.ac.uk/~klc/OntProg.html
    - In order to do something new, you must make sure that nobody else has done it.
    In 2018, there are 7 billion people.
    - We should not take pride in inventing something new.
    We should take pride in finding out that something is already known 50 years ago.
    - What is software engineering?
        - Engineering is the application of science.
        - Civil engineering is the application of natural science.
        - Software engineering is the application of computer science.
        - What is science?
            - Science is the application of the scientific method.
            - Science is a mixture of philosophy, mathematics, and experiments.
        - The ideal software is easy to change but doesn't change.
        The ideal software captures the essence of the problem.
        The essence of a problem is mathematical definitions.
        Mathematical definitions aren't supposed to change.
        - Software is a model of reality.
        - Software is law.
        - Software is executable theory of nature.
        - Software is like physics but executable.
        - https://queue.acm.org/detail.cfm?id=2693160
- Important things for all countries on Earth:
    - Change the justice system from retributive justice to restorative justice.
    Instead of locking up prisoners, make them repair the harm they did.
    - Who should we talk to?
    Legislators?
    - This is urgent and important.
        - Millions of people are being incarcerated.
        Their ability is being wasted.
        They are deprived of future.
        They will have difficulties reintegrating into society.
        Stigmatized.
        Like the homeless.
        This stigma makes it impossible for them to get financial stability and get out of the vicious circle of crime.
        - Not to mention questionable prison initiation rituals.
    - Social isolation causes violence and extremism.
        - [YT:I Was Almost A School Shooter | Aaron Stark | TEDxBoulder](https://www.youtube.com/watch?v=azRl1dI-Cts)
            - Bullying is part of the social isolation vicious circle.
- physics
    - physics expository works https://physics.stackexchange.com/questions/194300/physics-journals-that-focus-on-expository-work
    - "Newly published articles in physics" http://www.scholarpedia.org/article/Encyclopedia:Physics
- https://stackoverflow.com/questions/3416980/why-arent-whole-program-optimizations-more-prevalent-now/27757382
- https://en.wikipedia.org/wiki/Multi-adjoint_logic_programming
- https://medium.com/netflix-techblog/linux-performance-analysis-in-60-000-milliseconds-accc10403c55
- Keynote - What's Different In Dotty by Martin Odersky https://www.youtube.com/watch?v=9lWrt6H6UdE
- https://lifehacker.com/why-i-killed-my-standing-desk-and-what-i-do-instead-1565554537
