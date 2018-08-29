---
title: Deploying web applications
permalink: /deploy.html
date: 2018-08-29 19:09 +0700
---

- TOC
{:toc}

## Formalizing deployment requirements

What is the way to deploy web applications?

- General information
    - I have a Java web application.
    - It compiles by `mvn package`.
    - Its main class is `blah`.
- Network
    - It listens on port 1234.
    - Its URL should be `https://blah/`.
    - It is HTTPS only. HTTP port shouldn't be open at all.
- Resource requirements and burst characteristics
    - It needs 4 GB of RAM for acceptable garbage collection overhead.
    - It is mostly idle, but when it bursts, it requires 4 cores.
    - Ops is free to horizontally scale the stateless application server.

Assuming that I'm on either Amazon Web Services or Google Cloud Platform, how do I formalize my ops requirements in a cloud-agnostic way?
The 2016 article "On Formalizing and Identifying Patterns in Cloud Workload Specifications" [paywall](https://ieeexplore.ieee.org/document/7516840/) suggests an answer:

- "Approaches include orchestration specifications CAMP [1], [2], Open-CSA, SOA-ML and USDL,
and on the industrial side solutions such as Amazon CloudFormation, OpenStack Heat, Cloudify and Alien4Cloud.
To consolidate and enable interoperability within this variety of approaches, a technical committee by OASIS [3]
defined a standard for the Topology and Orchestration Specification of Cloud Applications (TOSCA) [4], [5],
which defines guidelines and facilities for the complete specification, orchestration and configuration of complex workloads,
addressing portability in heterogeneous clouds."

I assume that it suggests TOSCA.

Problem: The average person won't read a specification.

Who uses TOSCA?
Who implements that?
Why do I never see it on AWS or GCP?
Why would they follow your standard if they are the de facto standard?

Do we need more than one cloud providers?

Tools?

- kubernetes
- keter, pm2
- [Why Kubernetes is The New Application Server - RHD Blog](https://developers.redhat.com/blog/2018/06/28/why-kubernetes-is-the-new-application-server/)

Why I don't use NixOS:

- NixOS is insane.
Patching every software on Earth is not sustainable.
NixOS is only sustainable if the upstream developers use NixOS.

Who uses this?

- 2013 "Towards a Formal Model for Cloud Computing" [paywall](https://link.springer.com/chapter/10.1007/978-3-319-06859-6_34)

## Design

- 2018-08-30
- [Ontology, Infrastructure Classification, and the Design of Chef - Chef Blog](https://blog.chef.io/2015/04/23/ontology-infrastructure-classification-and-the-design-of-chef/)

I agree that 2018 devops ontologies suck, but I think we shouldn't avoid ontology-based systems.
The solution is not to avoid ontologies.
The solution is to craft a proper ontology that is timeless and essential.
This is a hard philosophical problem.

For example, the relationship between "application" and "entry point" is timeless.
"Entry point" is an essential property of "application".
By definition, every application has an entry point.

Every software has an implicit ontology, like it or not.

Every ontology systems that captures accident instead of essence is bound to fail.
Every computer ontology system that avoids philosophical ontology (What is X? What is the timeless essence of X?) is bound to fail.

Are these related?

- [WP:Existence precedes essence](https://en.wikipedia.org/wiki/Existence_precedes_essence)
- [WP:Essence](https://en.wikipedia.org/wiki/Essence) (probably unrelated to above)
- 2011 article "On doing ontology without metaphysics" [paywall](https://www.jstor.org/stable/41329478?seq=1#page_scan_tab_contents)
- 2012 article "Philosophies without ontology" [pdf](https://www.journals.uchicago.edu/doi/pdfplus/10.14318/hau3.1.015)
- [Philosophy is Bullshit: David Hume](https://webhome.phy.duke.edu/~rgb/Beowulf/axioms/axioms/node4.html)
    - What are pseudoquestions?

How do we answer "What is X?"?

There is an easy answer for mathematics.
Mathematics is unique in that its ontology is mostly a priori / by fiat: we say it exists; therefore it exists.
However, would it still be the case if we didn't have languages to express it?

For the real world it's hard.

Sometimes when we ask "What is X?", we are really asking "What is X for?" instead.

How do answer "What is X?" such that the correctness/truth/relevance of the answer does not depend on time/circumstances?
We don't know how to predict the far future.

## Continuous something

- Continuous integration
    - [Jenkins](https://jenkins.io/)
- Continuous delivery
    - [Spinnaker](https://www.spinnaker.io/)
- The ideal workflow: Git push triggers deployment?

## What is DevOps?

Separating Dev and Ops doesn't make sense.

Ops can't fix shitty Dev.
No amount of Ops will fix stupid programming.
Ops is impossible without decent Dev.

What is Google search result for "devops tools"?

- API description language, application description language: WADL vs Swagger vs what else?
    - https://www.w3.org/Submission/wadl/
    - 2010 article "DADL: Distributed Application Description Language" [pdf](https://www.isi.edu/~mirkovic/publications/dadlsubmit.pdf)
- ontology?
    - https://devops.stackexchange.com/questions/1361/what-are-known-efforts-to-establish-devops-ontology-model
    - 2016 article "Application of Ontologies in Cloud Computing: The State-Of-The-Art" [pdf available](https://arxiv.org/abs/1610.02333)
    - 2015 article "Composable DevOps" [paywall](https://dl.acm.org/citation.cfm?id=2867125)
    - 2012 article "Towards an Ontology for Cloud Services" [paywall](https://ieeexplore.ieee.org/document/6245776/)
    - 2012 article "Cloud Computing Ontologies: A Systematic Review" [pdf](https://pdfs.semanticscholar.org/cd5f/e6edb6284fcbcb470239464bb0c8e3ee2d50.pdf)
    - 2008 article "Toward a Unified Ontology of Cloud Computing" [pdf available](https://www.researchgate.net/publication/224367196_Toward_a_Unified_Ontology_of_Cloud_Computing)
    - https://www.skytap.com/blog/cloud-ontology/
    - OASIS TOSCA; too ad-hoc?
- what
    - 2015 article "Composable DevOps: Automated Ontology Based DevOps Maturity Analysis" [paywall](https://ieeexplore.ieee.org/document/7207405/)
