---
title: Intelligence
permalink: /intelligence.html
date: 2017-06-22 03:57:00 +0700
mathjax: true
---

- Abbreviations:
    - AI: Artificial Intelligence
    - ML: Machine Learning
    - COLT: Computational Learning Theory
- [About intelligence research]({% link intmeta.md %})
    - How can I become an AI researcher?
    - How are others' works progressing?
- [What is intelligence?]({% link intwhat.md %})
- What are some AI survey papers, review articles, and expository works?
    - Google query: most recent mathematical ai book
    - http://eliassi.org/COLTSurveyArticle.pdf
    - [WP: COLT surveys](https://en.wikipedia.org/wiki/Computational_learning_theory#Surveys)
    - https://mitpress.mit.edu/books/introduction-computational-learning-theory

## Plan

- Read about universal intelligence
    - Pamela McCorduck's "Machines who think" for some history
        - [WP: Timeline of artificial intelligence](https://en.wikipedia.org/wiki/Timeline_of_artificial_intelligence)
        - [WP: Progress in artificial intelligence](https://en.wikipedia.org/wiki/Progress_in_artificial_intelligence)
    - [Hutter2005Book]
    - [hutter1.net...uaibook.htm](http://www.hutter1.net/ai/uaibook.htm)
        - He formulated the "degree of intelligence" in 2005; we had a similar idea in 2018 in [intwhat.html]({% link intwhat.md %})
        - (edited) "AIXI [...] learns by eliminating Turing machines [...] once they become inconsistent with the progressing history."
    - [Presentation, 393 slides](http://www.hutter1.net/ai/suaibook.pdf)
    - [Slides](http://users.cecs.anu.edu.au/~ssanner/MLSS2010/Hutter1.pdf), maybe a draft of the above.
    - Shane Legg's PhD thesis "Machine super intelligence" [Legg2008]
    - [Legg and Hutter: A formal definition of intelligence for artificial systems](http://www.vetta.org/documents/universal_intelligence_abstract_ai50.pdf)
    - 2005 Negnevitsky AI book \cite{negnevitsky2005artificial}?

## Questions

- COLT
    - Should we read this?
        - [Ibrahim Alabdulmohsin: A Mathematical Theory of Learning](https://arxiv.org/abs/1405.1513)
        - 1999: [Sanjay Jain et al.: Systems that learn](http://www.cis.syr.edu/people/royer/stl2e/)
        - https://www.quora.com/What-are-the-best-math-books-for-machine-learning
        - https://machinelearningwithvick.quora.com/Learning-about-machine-learning
        - http://web.archive.org/web/20101102210231/http://measuringmeasures.com/blog/2010/1/15/learning-about-statistical-learning.html
        - https://www.quora.com/Which-are-the-best-books-to-get-the-Math-background-for-Machine-Learning
        - https://www.quora.com/How-do-I-learn-mathematics-for-machine-learning?share=1
    - http://emis.ams.org/journals/TAC/reprints/articles/22/tr22.pdf
        - https://www.quora.com/What-are-some-survey-papers-on-artificial-intelligence-and-deep-learning
        - http://people.idsia.ch/~juergen/deep-learning-conspiracy.html
        - [Jürgen Schmidhuber: "Deep Learning in Neural Networks: An Overview"](https://arxiv.org/abs/1404.7828)
        - http://www.ijircce.com/upload/2017/june/107_A%20Survey.pdf

Should we read these?

2017, [Building machines that learn and think for themselves](https://www.cambridge.org/core/journals/behavioral-and-brain-sciences/article/building-machines-that-learn-and-think-for-themselves/E28DBFEC380D4189FB7754B50066A96F)

## Note to self

- Which AI architecture has won lots of AI contests lately?
    - Is it LSTM RNN?
    - What is LSTM RNN?
        - "long short-term memory recurrent neural network"
        - http://colah.github.io/posts/2015-08-Understanding-LSTMs/
        - "The expression *long short-term* refers to the fact that LSTM is a model
        for the *short-term memory* which can last for a *long* period of time." ([Wikipedia](https://en.wikipedia.org/wiki/Long_short-term_memory))
- How do we learn amid lies, deception, disinformation, misinformation?
    - Related to adversarial learning? https://en.wikipedia.org/wiki/Adversarial_machine_learning ?
- What are some tools that I can use to make my computer learn?
    - Google TensorFlow
    - Does OpenAI have tools?
- TODO s/adapt/habituate
- Let \\( f(t,x) \\) be the system's response intensity for stimulus intensity \\( x \\) at time \\( t \\). We say the system is *habituating* between the time \\( t_1 \\) and \\( t_2 \\) iff \\( f(t_1,x) > f(t_2,x) \\) for all stimulus intensity \\( x \\).
- "The habituation process is a form of adaptive behavior (or neuroplasticity) that is classified as non-associative learning." https://en.wikipedia.org/wiki/Habituation
- How many AI approaches are there?
    - [WP AI Portal](https://en.wikipedia.org/wiki/Portal:Artificial_intelligence) lists 4 approaches
    - Pedro Domingos lists 5 "tribes"
- (merge AI researchers)
    - [WP AI Portal](https://en.wikipedia.org/wiki/Portal:Artificial_intelligence) lists several leading AI researchers
- Why is consciousness trapped in a body? Why can't we swap the consciousness of two people?
- 2000, György Turán, [Remarks on COLT](https://link.springer.com/article/10.1023%2FA%3A1018948021083)
- 2016, Krendzelak, Jakab, [Fundamental principals of Computational Learning Theory](https://ieeexplore.ieee.org/document/7802092/)
    - Reading queue:
        - D. Angluin, C. Smith, "Inductive inference: theory and methods", A.C.M. Computing Surveys, vol. 15, pp. 237-269, 1983.
        - M. Anthony, N. Biggs, "Computational Learning Theory" in , Cambridge university press, 1992.
        - M.J. Kearns, "The computational Complexity of Machine Learning" in , The MIT Press, May 1990.
        - L.G. Valiant, "A theory of the learnable", Communications of the A.C.M., vol. 27, no. 11, pp. 1134-1142, 1984.
        - L. Pitt, L.G. Valiant, "Computational limitations on learning from examples", Journal of the A.C.M., vol. 35, no. 4, pp. 965-984, 1988.
- helpful slides
https://cs.uwaterloo.ca/~klarson/teaching/W15-486/lectures/22Colt.pdf
- Bertoni et al.
http://elearning.unimib.it/pluginfile.php/283303/mod_resource/content/1/Apprendimento_Automatico/Computational_Learning.pdf
- https://stats.stackexchange.com/questions/142906/what-does-pac-learning-theory-mean
- https://pdfs.semanticscholar.org/presentation/fbbd/65646c8a81094864d4e0b0dfb9c1f22181af.pdf
- http://web.cs.iastate.edu/~honavar/colt-tutorial.pdf
- https://en.wikipedia.org/wiki/Probably_approximately_correct_learning#cite_note-valiant-1
A Theory of the Learnable
Leslie G. Valiant
1984
http://web.mit.edu/6.435/www/Valiant84.pdf
- kearns vazirani introduction
ftp://ftp.cis.upenn.edu/pub/cse140/public_html/2002/kvpages.pdf
- http://www.cis.upenn.edu/~mkearns/
the computational complexity of machine learning
http://www.cis.upenn.edu/~mkearns/papers/thesis.pdf
https://www.worldscientific.com/worldscibooks/10.1142/10175
- 2015
http://www.cs.tufts.edu/~roni/Teaching/CLT/
- probably link to this
http://bactra.org/notebooks/learning-theory.html
- semantics-first
https://pdfs.semanticscholar.org/83e7/b615c165209af54dd0fe05c850bb08232625.pdf
- discrete approximation theory
see the references of this paper
https://www.worldscientific.com/doi/suppl/10.1142/10175/suppl_file/10175_chap01.pdf
- https://profs.info.uaic.ro/~ciortuz/SLIDES/ml7.pdf

Optimal learning for humans
https://www.kqed.org/mindshift/37289

Curate from this
https://thesecondprinciple.com/optimal-learning/

Boston dynamics dog robots

Tesla car autopilots

Google and Uber self-driving cars

https://www.quora.com/Will-we-ever-have-a-rigorous-and-robust-definition-for-intelligence

rigorous definition of intelligence
The new ai is general and rigorous, idsia
Toward a theory of intelligence,RAND

A system responds to a stimulus.
Define: a system is *adapting* to a stimulus if the same stimulus level elicits decreasing response level from the system.
The stimulus level has to be increased to maintain the response level.

Is learning = adapting?
Is intelligence = adaptiveness?

## Others

- What are some expository works in AI?
    - [The evolution of sentiment analysis—A review of research topics, venues, and top cited papers](https://www.sciencedirect.com/science/article/pii/S1574013717300606)
- What are the trends in AI?
    - [Michael Nielsen's tweet](https://twitter.com/michael_nielsen/status/983502409325395969):
    "I meet lots of people who tell me fatalistically (& often despondently) that it's near impossible to do important work on neural nets today, unless you have huge compute and huge data sets."
        - [Deep Learning Scaling is Predictable, Empirically](https://arxiv.org/abs/1712.00409)
- Should we read this?
    - [Boosting: Gradient descent in function space](http://www.cs.cmu.edu/~16831-f12/notes/F11/16831_lecture15_shorvath.pdf)
    - [Alessio Guglielmi's deep inference](http://alessio.guglielmi.name/res/cos/)
    - [Problem theory, Ramón Casares](https://arxiv.org/abs/1412.1044)
- EcoBot is a robot that can feed itself.
    - [Wikipedia: EcoBot](https://en.wikipedia.org/wiki/EcoBot):
    "a class of energetically autonomous robots that can remain self-sustainable
    by collecting their energy from material, mostly waste matter, in the environment"
- [A single-celled organism capable of learning](https://www.sciencedaily.com/releases/2016/04/160427081533.htm): protists may learn by habituation
- Selected threads from /r/artificial:
    - [What are some of the best books on AI/ML?](https://www.reddit.com/r/artificial/comments/8begcv/what_are_some_of_the_best_books_on_artificial/)
    - [Math PhD. Want to learn more about AI. What to read?](https://www.reddit.com/r/artificial/comments/8bzrmd/math_phd_want_to_learn_more_about_ai_what_to_read/)
- What is so bad about human extinction?
    - If you are nihilist, then there is nothing inherently bad about human extinction.
- What is the question?
- How do we make an AI?
- How do we create a seed AI?
- History questions:
    - Why was Raymond J. Solomonoff \cite{SolAlpProb2011, GacsVitanyiSolomonoff} interested in predicting sequences of bits?
    What was he interested in?
    What was he trying to do?
- Mathematical spaces
    - What is a metric?
    - What is a norm?
    - What is a measure?
    - https://en.wikipedia.org/wiki/Space_(mathematics)#Three_taxonomic_ranks
    - https://en.wikipedia.org/wiki/Topological_space#Classification_of_topological_spaces
    - https://en.wikipedia.org/wiki/Functional_analysis
        - What is a Hilbert space?
        - What is a Banach space?
        - What is a Sobolev space?
        - What is a measure?
            - What is a Lebesgue measure?
                - What is an Lp space?
                    - [Wikipedia: Lp space](https://en.wikipedia.org/wiki/Lp_space#Lp_spaces)
                    - How is it pronounced?
                        - "Lebesgue space with \\(p\\)-norm"
                - What is a small lp space?

## Non-prioritized questions

- What is AI? Why should I care?
    - AI is the way for us to become gods.
- What is the relationship between AI and ML?
    - ML is a subset of AI.
        - Then what is the rest of AI that is not ML?
            - Ethics? Philosophy? Rule systems?
            - [AI SE 35: What is the difference between artificial intelligence and machine learning?](https://ai.stackexchange.com/questions/35/what-is-the-difference-between-artificial-intelligence-and-machine-learning)
            - What is intelligence without learning?
            Non-adaptive intelligence? Static intelligence?
- What is a cyborg?
- If human goal function is survival, then why exists suicide?
    - Evolutionary noise?

https://en.wikipedia.org/wiki/Universal_Darwinism

## How might we build a seed AI?

- Use off-the-shelf computers.
- Use supercomputers.
- Use clusters.
- Use computers over the Internet.
- Raise an AI like raising a child.
- Evolve a system. Create an environment with selection pressure. Run it long enough.
    - [WP: Evolutionary robotics](https://en.wikipedia.org/wiki/Evolutionary_robotics)
    - [WP: Evolutionary computation](https://en.wikipedia.org/wiki/Evolutionary_computation)
- What is TensorFlow? Keras? CNTK? Theano?
    - The building blocks of AI? Standardized AI components?

## Guesses

In the future, there are only two kinds of jobs:
telling machines to do things,
and being told to do things by machines.
