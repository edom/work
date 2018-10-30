---
title: Security
permalink: /security.html
date: 2018-05-15 02:31:00 +0700
---

# Security

- Bastion hosts, aka jump boxes
    - Does a jump box add any security?
        - http://cloudacademy.com/blog/aws-bastion-host-nat-instances-vpc-peering-security/
        - http://www.infoworld.com/article/2612700/security/-jump-boxes--improve-security--if-you-set-them-up-right.html
- Check your HTTPS
    - [Test your HTTPS implementation](https://www.ssllabs.com/ssltest/); it's too easy to do security wrong.
- [HashiCorp Vault](https://www.vaultproject.io/)
    - [source code](https://github.com/hashicorp/vault), language Go, license MPL 2.0
    - What is Vault? "Vault is a tool for securely accessing *secrets*." ([Introduction](https://www.vaultproject.io/intro/index.html))
    - What can it do?
    - How do I install it?
    - How do I run it?
    - How do I interact with it?
    - [HashiCorp](https://www.hashicorp.com/)
- [SE 5930529: How is revocation of a root certificate handled?](https://stackoverflow.com/questions/5930529/how-is-revocation-of-a-root-certificate-handled)
- [WP: Online Certificate Status Protocol](https://en.wikipedia.org/wiki/Online_Certificate_Status_Protocol)
- [WP: OCSP Stapling](https://en.wikipedia.org/wiki/OCSP_stapling) moves the cost from client to server.
- Zero trust security model (ZTSM)
    - "How would I design my system without any firewalls?"
    - https://www.scaleft.com/beyondcorp/
        - old approach: perimeter security, medieval castle, weak core, strong perimeter
        - https://storage.googleapis.com/pub-tools-public-publication-data/pdf/43231.pdf
            - "The perimeter security model works well enough when all employees work exclusively in buildings owned by an enterprise."
            - "access depends solely on device and user credentials, regardless of a user’s network location"
            - "All access to enterprise resources is fully authenticated, fully authorized, and fully encrypted based upon device state and user credentials."
            - ZTSM obviates VPN (virtual private network).
