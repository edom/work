---
title: Intelligence
permalink: /intelligence.html
date: 2017-06-22 03:57:00 +0700
mathjax: true
---

I think we should drop the term "artificial intelligence" and just say "intelligence".

Reading triage

Learning, and approximation theory [Poggio1989]

Intelligent agents are only possible if the world they live in is structured.
If the laws of physics randomly change over time,
then intelligent agents likely won't exist.

How do we make an AI?

[Michael Nielsen's tweet](https://twitter.com/michael_nielsen/status/983502409325395969):
"I meet lots of people who tell me fatalistically (& often despondently) that it's near impossible to do important work on neural nets today, unless you have huge compute and huge data sets."

[Deep Learning Scaling is Predictable, Empirically](https://arxiv.org/abs/1712.00409)

Expository:
[The evolution of sentiment analysis—A review of research topics, venues, and top cited papers](https://www.sciencedirect.com/science/article/pii/S1574013717300606)

"To learn X" is to get better at X by using experience.

AI
deepmind
wavenet
DEEPCODER: LEARNING TO WRITE PROGRAMS
https://openreview.net/pdf?id=ByldLrqlx

AI is basically approximation (or constrained optimization?) in Sobolev spaces (or \( L^p(\Real) \) spaces?)

http://math.bu.edu/people/mkon/V5Fin.pdf
Extending Girosi’s approximation estimates for functions in Sobolev spaces via statistical learning theory
Mark A. Kon, Louise A. Raphael, Daniel A. Williams
"Girosi [8] established an interesting connection between statistical learning theory
(SLT) and approximation theory, showing that SLT methods can be used to
prove results of a purely approximation theoretic nature."

[Jacques Pitrat](http://jacques.pitrat.pagesperso-orange.fr/) and his CAIA,
bootstrapping AI with AI.

In 2007, on page 12, in the paper [Universal intelligence: a definition of machine intelligence](https://arxiv.org/pdf/0712.3329.pdf),
Shane Legg and Marcus Hutter wrote,
"Intelligence measures an agent’s ability to achieve goals in a wide range of environments,"
and then they went on to formalize that definition,
but I don't understand, perhaps because I skimmed too impatiently,
but I find that informal definition insightful.
Here I try another formalization.

Let \\(E\\) be a set of *environments*.

Let \\(G : E \to \Real\\) be a *goal function*.
The value of \\(G(e)\\) measures how well the agent performs in environment \\(e\\).

The *intelligence* of the agent *with respect to \\(G\\) across \\(E\\)* is \\( \int_E G \\).

A *performance* consists of an agent and an environment.

Assumption:
The agent cannot modify \\(G\\).

Behavior is a function taking an environment and outputing something.

Intelligence is *relative* to \\(G\\) and \\(E\\): *goal* and *environment*.

If we see longevity as intelligence test,
then an illiterate farmer who lives to 80
is more intelligent than a scientist who dies at 20,
but a rock that has been there for 100 years would even be more intelligent than the farmer.

If we see money as intelligence test,
then a corrupt politician who steals billions of dollars without getting caught
is more intelligent than a honest farmer who only has tens of thousands of dollars.

Gaming the system, however undesirable, is nevertheless a sign of intelligence.

It is hard to design a goal function that gives the desired outcome without undesired side effects.

IQ tests are intelligence measures with small environment set.

Lifespan may be an intelligence measure with huge environment set.

A human can optimize *several* goal functions across the same environment set.
A human may be asked to clean a floor, to write a report, to run a company, to cook food,
and to find the quickest route between home and office,
and optimize them all.

Some goal functions for humans are (but perhaps not limited to):

- Maximize happiness
- Minimize pain
- Optimize the level of a chemical in the brain
- Optimize the time integral of such chemical
- Maximize the chance of survival

but I don't know the root goal function
that explains all those behaviors.

## Summary of Pedro Domingos' "The master algorithm"

Sparse autoencoders (p. 116).

Hume's question:
How do we justify generalization?
Why generalization works??

Wolpert answers Hume question in "no free lunch theorem"?

"A nugget of knowledge so incontestable, so fundamental, that we can build all induction on top of it" (p. 64) in Chapter 9.

Induction is the inverse of deduction,
as subtraction is the inverse of addition.

Christianity as we know it was invented by Saint Paul, while Jesus
saw himself as the pinnacle of the Jewish faith. (p. 144)

EM (expectation maximization) algorithm (p. 209).

Metalearning (p. 237).
A classifier that classifies by combining the output of subclassifiers.

[Markov logic network](http://homes.cs.washington.edu/~pedrod/papers/mlj05.pdf) (p. 246) named [http://alchemy.cs.washington.edu/](Alchemy) (p. 250)

## (2017-02-26) An agent in a discrete world

\chapter{Agent models}

\newcommand\Sta{\fun{Sta}}
\newcommand\law{\fun{law}}

\section{Explicit-world mono-unary algebra}

A \emph{world} \(W\) is a mono-unary algebra \((\Sta~W, ~ \law~W)\)
where \(\Sta~W\) is the \emph{world state type}
and \(\law~W : \Sta~W \to \Sta~W\) is the \emph{world law}.

An \emph{agent} \(A\) is a mono-unary algebra \((\Sta~A, ~ \law~A)\)
where \(\Sta~A\) is the \emph{agent state type}
and \(\law~A : \Sta~A \to \Sta~A\) is the \emph{agent law}.

The \emph{environment} is the world minus the agent.
Something is a part of the agent if and only if
the agent can directly control that part.
Otherwise it is a part of the environment.

\newcommand\fpenalty{\fun{penalty}}
\newcommand\reward{\fun{reward}}
\newcommand\sense{\fun{sense}}
\newcommand\actuate{\fun{actuate}}

The function \(\sense : \Sta~W \to \Sta~A\)
defines how the agent perceives the world.
This function is not invertible
because \(\Sta~W \supset \Sta~A\).
This means that \emph{there exists part of the world that the agent cannot sense.}

An agent exists in a world.
The \(\sense\) function must be a homomorphism from the world to the agent.
The \(\sense\) function must satisfy this equation:
\begin{equation}
    \sense \circ \law~W = \law~A \circ \sense.
\end{equation}

That equation relates the actual world law
and what the law looks like from the agent's point of view.
The agent can never know the world law.
The agent can only discover something homomorphic to that law.
That means \emph{we can never know the laws of nature}.
We will never know the reality.
We can only know something homomorphic to the laws of nature.

\newcommand\orbit{\fun{orbit}}
\newcommand\Orbit{\fun{Orbit}}
\newcommand\InfSeq{\fun{InfSeq}}
\newcommand\iterate{\fun{iterate}}
\newcommand\judge{\fun{judge}}

Starting from a state \(x\), the agent forms the sequence
\( \orbit~A~x = \iterate~(\law~A)~x = (x, ~ f~x, ~ f^2~x, \ldots) \) where \(f = \law~A\).
We define such \(\orbit~A~x\) to be a member of the type \(\Orbit~A\).
We use the notation \(\InfSeq~A\) to mean
the space of infinite sequences
where each element has type \(A\).
We have \(\Orbit~A \subset \InfSeq~A\).
We consider the type \(\Orbit~A = \{ \orbit~A~x ~|~ x : \Sta~A \}\),
the set of all orbits of \(A\).
We have a judge function that judges an orbit.
This function is \(\judge : \Orbit~A \to \Real\).

Now we assume that every \(x : \Sta~A\) is distributed uniformly.
Define \(p~r\) as
the probability of finding an \(x\) where \(\judge~x \le r\).
The shape of the distribution \(p\)
describes the intelligence of the agent.

The function \(\fpenalty : \Sta~A \to \Real\)
defines the undesirability of an agent state.
Alternatively, the function \(\reward : \Sta~A \to \Real\)
defines the desirability of an agent state.
The function measures how bad or how good the agent performs.
This is the agent's hidden objective function.
This is hardwired.
This is arbitrary.
The agent doesn't have to be aware of this.
An intelligent agent acts to make its
\(\fpenalty~x\) as close to zero as possible
in the long term for as many \(x\) as possible,
where \(x\) is an agent state.

The agent displays an intelligent behavior
if it can minimize the long-term penalty from lots of starting states.
The most intelligent agent is the one that minimizes its lifelong sum of penalty?

An agent has input and output.

An \emph{agent logic} is a function of type \((M,I) \to (M,O)\)
where \(M\) is the memory type,
\(I\) is the input type,
and \(O\) is the output type.
We assume that the world remembers the agent memory.

\section{Limitations}

The agent is not omniscient.
The agent does not know everything.
The agent can only perceive a small part of the world.
The agent has physical limitations.
The agent cannot know the whole world.

\section{Implicit-world discrete dynamical system model}

Let \(w\) be a world.
Let \(a\) be an agent in world \(w\).
Let \(x~t\) be the input of the agent at time \(t\).
Let \(y~t\) be the output of the agent at time \(t\).
Let \(m~t\) be the memory of the agent at time \(t\).

We assume that the agent needs one time step to compute the output.
\begin{align}
    y~(t+1) &= Y~(x~t)~(m~t)~t
    \\
    m~(t+1) &= M~(x~t)~(m~t)~t
    \\
    x~(t+1) &= X~(x~t)~(y~t)~t
\end{align}

\section{Dynamical systems}

\section{Measuring the intelligence of a phase space trajectory}

We can think of a human as a dynamical system.

Given two phase space trajectories,
which is more intelligent?
Why?
The most intelligent is the most homeostatic, the most stabilizing, the most controlling.

## Discrete agent model

Let \\(w\\) be the world law,
and \\(s\\) be short-term goal function.
At time \\(t\\),
let \\(x_t\\) be agent input,
\\(y_t\\) be agent output,
\\(a_t\\) be the agent logic,
Assume that these equations hold:

<div>\begin{align*}
y_t &= a_t(x_t)
\\ x_{t+1} &= w(x_t,y_t)
\\ g_t &= s(x_t).
\end{align*}</div>

Rearranging and simplifying gives:

<div>\begin{align*}
\\ g_{t+1} &= s(x_{t+1})
\\ &= s(w(x_t,y_t))
\\ &= s(w(x_t,a_t(x_t)))
\end{align*}</div>

<div>\begin{align*}
\end{align*}</div>

The goal function takes \\(e_N\\) and outputs a number,
where \\(N\\) is a constant.

The agent wants to maximize \\(\sum_{t=0}^\infty g_t\\).

## Ramble

World = agent + environment.
Environment is everything that the agent does not control directly.
The body of an agent is part of the environment, not of the agent.

[Dimension independent similarity computation (DISCO)](http://dl.acm.org/citation.cfm?id=2567715)

[Journal of artificial intelligence research](http://www.jair.org/) (open access)

[Machine theory of mind](https://www.semanticscholar.org/paper/Machine-Theory-of-Mind-Rabinowitz-Perbet/4a48d7528bf1f81f48be8a644ffb1bcc08f1b2c5)
