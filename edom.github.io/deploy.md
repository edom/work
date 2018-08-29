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
