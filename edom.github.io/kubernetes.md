---
title: Kubernetes
date: 2018-05-23 00:00 +0700
permalink: /kubernetes.html
---

- [kubernetes.io](https://kubernetes.io/): "Production-Grade Container Orchestration"
- How do we control access to Kubernetes?
    - https://stackoverflow.com/questions/42170380/how-to-add-users-to-kubernetes-kubectl
    - https://kubernetes.io/docs/admin/authentication/
    - https://kubernetes.io/docs/admin/accessing-the-api/
    - How do we add and remove users and roles to Kubernetes?
    - Which is the most hassle-free future-proof minimal-maintenance way?
- Kubernetes security
    - https://kubernetes.io/blog/2016/08/security-best-practices-kubernetes-deployment/
    - https://kubernetes.io/docs/tasks/administer-cluster/securing-a-cluster/
    - Which document should we read? Overlapping? Confusing?
        - https://kubernetes.io/docs/reference/access-authn-authz/controlling-access/
        - https://kubernetes.io/docs/reference/access-authn-authz/authentication/
        - https://kubernetes.io/docs/reference/access-authn-authz/authorization/
    - What is Kubernetes's replacement of AWS security groups?
        - NetworkPolicy objects
            - https://kubernetes.io/blog/2017/10/enforcing-network-policies-in-kubernetes/
            - https://kubernetes.io/blog/2016/04/kubernetes-network-policy-apis/
- How I think Kubernetes fits in Google's strategy
    - Kubernetes commoditizes IaaS providers.
        - It lowers the barrier of switching from any other cloud providers to GCE.
            - Examples of other cloud providers:
            Amazon Web Services (AWS), DigitalOcean, Alibaba Cloud (Aliyun), Microsoft Azure
        - The same way Microsoft Windows commoditized PC hardware.
    - [WP:Commoditization](https://en.wikipedia.org/wiki/Commoditization)
    - [WP:Infrastructure as a service](https://en.wikipedia.org/wiki/Infrastructure_as_a_service)
