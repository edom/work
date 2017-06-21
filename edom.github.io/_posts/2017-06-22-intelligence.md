---
title: Intelligence
permalink: /intelligence.html
date: 2017-06-22 03:57:00 +0700
mathjax: true
---

In 2007, on page 12, in the paper [Universal intelligence: a definition of machine intelligence](https://arxiv.org/pdf/0712.3329.pdf),
Shane Legg and Marcus Hutter wrote:

> Intelligence measures an agent’s ability to achieve goals in a wide range of
environments.

and then they went on to formalize that definition,
but I don't understand, perhaps because I skimmed too impatiently,
but I find that informal definition insightful.
Here I try another formalization.

Let \\(E\\) be a set of *environments*.

Let \\(G : E \to \Real\\) be a *goal function*.
The value of \\(G(e)\\) measures how well the agent performs in environment \\(e\\).

The *intelligence* of the agent *with respect to \\(G\\) across \\(E\\)* is \\( \int_E G \\).

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
