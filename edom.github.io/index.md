---
layout: default
date: 2017-05-20 00:00:00 +0700
---

- TOC
{:toc}

## Welcome and disclaimers

I write everything for myself, but I hope you find it useful.

I may err.
Read cautiously.
Compare with other independent sources.

I am not responsible for other people's comments.

Organization of this site:
For most pages, the title is a goal, and the content is the ongoing journey to achieve it.

## Goals

- Computer practice
    - Software engineering
        - Forward software engineering
            - [Making lasting software sooner]({% link softeng.md %})
            - [Making the only programming language we will ever need]({% link plres.md %})
                - [Extending lambda calculus]({% link lambda.md %})
                - [Extending Haskell]({% link exhask.md %})
                - [Programming language design mistakes]({% link pldm.md %})
                - Making interpreters/translators/compilers
                    - Don't make a compiler?
                    Make an interpreter instead, and stage it?
                    Turn an interpreter into a compiler for free?
                    - [Parsing]({% link parse.md %})
                - [Modeling]({% link model.md %})
                    - [Modeling all data]({% link modeldata.md %})
                        - [Solving the AST decoration problem]({% link ast.md %})
                        - Solving the expression problem
                - Functional programming
                    - [Optimizing lambda calculus]({% link optlam.md %})
                - Managing large software
                    - [Designing module systems]({% link module.md %})
                    - [Argument for static typing]({% link statyp.md %})
                    - [Proving things using computers]({% link proof.md %})
                    - Maintaining backward compatibility
                        - Making only backward-compatible changes simplifies the lives of people who depend on you.
                        - Too backward-compatible?
                            - GNU Autotools
                            - Windows 95: [Much more than you would ever know.. The original version of Sim City was writt... \| Hacker News](https://news.ycombinator.com/item?id=2281932)
                - [Designing markup languages]({% link markup.md %})
                - [Designing configuration languages]({% link conflang.md %})
            - Using programming languages and their related tools
                - [Using Haskell]({% link haskell.md %})
                    - [Using Cabal]({% link cabal.md %})
                - [Using Java]({% link java.md %})
                    - Building a Java project
                        - 2018-08-29: If you have to choose between Maven and Gradle, and your code is 100% Java, then I recommend Maven.
                        - [Using Maven]({% link maven.md %})
                        - [Using Gradle]({% link gradle.md %})
                - [Using Python]({% link python.md %})
            - [Using XML]({% link xml.md %})
            - Using Emacs
                - [Vivek Haldar — The levels of Emacs proficiency](http://blog.vivekhaldar.com/post/3996068979/the-levels-of-emacs-proficiency)
            - [Making contributable open-source projects]({% link opensrc.md %})
                - [Building online communities]({% link community.md %})
                - Maintaining open-source projects
            - [Deploying web applications]({% link deploy.md %})
            - [Making a Java virtual machine]({% link jvm.md %})
        - [Reverse software engineering]({% link reveng.md %})
            - [Reverse engineering the network protocol used by a Java desktop application]({% link rejava.md %}): a stock trading application written in Java 6
            - [Reverse-engineering PlayStation 1 3D games to control the camera]({% link ps1.md %})
        - [Managing software engineers]({% link engman.md %})
            - [Interviewing software engineers]({% link engint.md %})
            - [Reviewing performance]({% link perfrev.md %})
        - [Software engineer salary]({% link salary.md %})
    - [Administering a computer]({% link sysadm.md %})
        - [Running X client applications on Docker on Linux]({% link dockerx.md %})
    - [Using computers]({% link usecom.md %})
        - [Searching the Internet]({% link search.md %})
        - [Removing nag screens]({% link nag.md %})
        - [Making a personal wiki]({% link perswiki.md %})
        - [Buying a smartphone]({% link phone.md %})
            - [Android vs iPhone]({% link andvsiph.md %})
        - [Contributing to Wikipedia]({% link wikipedia.md %})
    - [Liberating software]({% link libersoft.md %})
        - Searching
            - Where does this fit in the hierarchy?
            - [WP:Optimal stopping](https://en.wikipedia.org/wiki/Optimal_stopping)
            - [Making a search engine]({% link srceng.md %})
- Computer science
    - Intelligence
        - Doing the last work we will ever need
            - [Defining intelligence]({% link intwhat.md %})
            - [Making intelligence]({% link intelligence.md %})
            - Making machines understand language
            - How do we make machines curious?
            How do we make them get bored?
                - We know that intelligent people get bored quickly.
                    - Why shouldn't intelligent machines get bored?
            - [Approximating functions]({% link approx.md %})
    - Computational complexity
        - [Trying to prove P neq. NP]({% link pnptry.md %})
- [Getting rich]({% link getrich.md %})
    - [Investing]({% link invest.md %})
    - [Justifying my value]({% link value.md %})
- Managing organizations
    - [Spreading information in an organization]({% link spreadinf.md %})
    - 2014, article, "Whose critical path are you on?", Philip J. Guo, [html](http://www.pgbovine.net/critical-path.htm)
        - To get someone's attention, get on his critical path.
    - [Netflix culture](https://jobs.netflix.com/culture)
- Understanding nature
    - [Justifying physics]({% link natkno.md %})
    - [Justifying counterfactual reasoning]({% link cf.md %})
- Improving society
    - [Legislating better societies]({% link government.md %})
    - [Politics]({% link politics.md %})
        - [Solving the Israel-Palestine problem]({% link ispal.md %})
        - [Using the media in politics]({% link polmed.md %})
    - Preventing social isolation
        - Social isolation causes violence and extremism.
        Bullying is part of the social isolation vicious circle.
        [YT:I Was Almost A School Shooter \| Aaron Stark \| TEDxBoulder](https://www.youtube.com/watch?v=azRl1dI-Cts)
        - [YT:Cohousing communities help prevent social isolation](https://www.youtube.com/watch?v=DmWrx0ntATU)
    - Achieving independence
        - [Achieving energy independence]({% link energy.md %})
        - Demonopolizing the ability to do violence?
            - People who care (and thus readily mobilize) are less oppressable than people who don't.
                - How do we get people to care about and participate in politics?
            - Online campaigns/petitions.
                - [Technology can give political power back to the people - New Scientist](https://www.newscientist.com/article/mg22630182-800-technology-can-give-political-power-back-to-the-people/)
            - Using the Internet (social media, messaging, forum, etc.) to incite people?
            - Teach people how to participate democracy?
                - If shit gets to the top, people have only themselves to blame.
                    - Democracy means they could have prevented that shit.
                        - But they were apathetic.
                        They didn't care.
                        They gave up before even trying.
                        So that shit rose without any obstacles.
        - Automating politics/governments/law?
        - Empowering people with distributed local technology?
            - Take power away from governments and elites, and give it back to the masses?
                - Isn't that mobocracy/ochlocracy?
                Anarchy?
                Do we want?
                    - [RationalWiki: Difference between anarchy and mobocracy](https://rationalwiki.org/wiki/Anarchy#Contrast_with_mobocracy)
- [Doing research better]({% link meta_research.md %})
    - [Organizing knowledge]({% link knorg.md %})
        - [Rebooting human knowledge in case of mass destruction]({% link reboot.md %})
        - The page [Information architecture]({% link infarch.md %}) should be merged into "Organizing knowledge".
- Living in the 21st century
    - [What kind of world are we living in?]({% link world.md %})
    - Finding a place to live
        - [Where is the world's most liveable city? \| The Economist - YouTube](https://www.youtube.com/watch?v=ylR21fezN7E)
            - 2018: Vienna. Was Melbourne.
        - Making a house
            - [WP:List of human habitation forms](https://en.wikipedia.org/wiki/List_of_human_habitation_forms)
    - Grooming, styling, appearance
        - [WP:Hairstyles](https://en.wikipedia.org/wiki/List_of_hairstyles)
            - [Wearing long hair for men]({% link longhair.md %})
        - [Naming articles of clothing]({% link cloth.md %})
    - Health and fitness
        - [Growing muscles]({% link muscle.md %})
        - [Mouth care]({% link mouth.md %})
        - [Eating]({% link eat.md %})
            - [How to cook eggs]({% link egg.md %})
    - Hidup di Indonesia abad 21
        - [Politik Indonesia]({% link politik.md %})
            - [Censoring the Internet in Indonesia]({% link indocen.md %})
            - Memakai hak warga negara
                - BPJS?
                    - [Cek saldo BPJS](https://daftar.bpjs-kesehatan.go.id/bpjs-checking/)
                    - [Ini registrasi apa?](https://sso.bpjsketenagakerjaan.go.id/registrasi.bpjs)
                    Kenapa harus registrasi?
                - [BTPN Jenius?]({% link btpn.md %})
                - Mendirikan perusahaan?
                    - Bagaimana?
                    - Bayar berapa?
                    - Berapa lama?
                    - Apa keuntungan pajak?
                - Jangan abstain dalam pemilu.
                Lihat argumen [/u/kolormelar](https://www.reddit.com/r/indonesia/comments/968qbs/ajakan_untuk_yang_tidak_cocok_dengan_pasangan/e3yw8bd/).
                - Pemerintah menyediakan akses ke jurnal berbayar? Di mana? Cara pakainya?
        - [Living in Jakarta]({% link jakarta.md %})
            - [Vital service failure log]({% link log.md %})
    - [Handling existential crisis]({% link crisis.md %})
    - [Fighting]({% link fight.md %})
    - Dying in the 21st century
        - Inheriting digital data
            - What to do with your data when you die?
            - https://www.businessinsider.sg/how-to-give-google-account-to-trusted-person-when-you-die-2018-8/
        - Do we have to die?
        Can technology save us?
        Will we reach immortality in this century?
- [Mathematics]({% link math.md %})
    - [Learning mathematics]({% link mathlearn.md %})
    - [Teaching mathematics]({% link mathedu.md %})
    - [Generalizing division]({% link division.md %})
    - [Solving the Clay millennium prize problems]({% link clay_math.md %})
    - Writing mathematics
        - 1989, article, "Mathematical writing", Donald E. Knuth, Tracy Larrabee, and Paul M. Roberts, [pdf](http://jmlr.csail.mit.edu/reviewing-papers/knuth_mathematical_writing.pdf)
            - "Many readers will skim over formulas on their first reading of your exposition.
            Therefore, your sentences should flow smoothly when all but the simplest formulas are replaced by 'blah' or some other grunting noise. (p. 3)
- [Writing]({% link writing.md %})
    - [Working around Markdown]({% link markdown.md %})
- Spirituality
    - [Choosing a religion]({% link relch.md %})
    - [Obtaining magickal powers]({% link magick.md %})
        - [Experimenting with magick]({% link magexp.md %})
- Learning languages
    - [Learning English]({% link english.md %})
    - [Learning Mandarin]({% link mandarin.md %})
    - [Learning Japanese]({% link japanese.md %})
    - Bahasa Indonesia
        - Apa beda manjur, mempan, mujarab, dan mustajab?
- [Accounting]({% link account.md %})
- Finding other information sources
    - Finding other wikis
        - [WP:List of wikis](https://en.wikipedia.org/wiki/List_of_wikis)
- [Other unorganized contents]({% link other.md %})
    - [Research]({% link research.md %})
- Music
    - I play piano in the band [The Nomads]({% link nomads.md %}) (an Indonesian band, not the Swedish band).
- [About this website]({% link about.md %})
- [Things I've forlet]({% link forlet.md %}): goals I've abandoned

## Contact

To ask questions or suggest corrections, do any of these:

- Leave a Disqus comment in the related page.
- [Open an issue on GitHub](https://github.com/edom/edom.github.io/issues).
