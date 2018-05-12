---
title: Cryptocurrency
permalink: /crycur.html
date: 2016-01-01 00:00:00 +0700
---

- Blockchain, cryptocurrency, distributed ledger
    - What is blockchain?
    - What is cryptocurrency?
    - What is distributed ledger?
    - How are they related?
    - Is a Git repository a Merkel tree?
        - ??? Merkelization of a data structure is adding a cryptographic hash to every node?
- A possible cryptocurrency design?
    - A cryptocurrency that proves that W work has been done by A for B, anonymously. Is it possible?
    - Every principal has a keypair
    - Every principal creates a debt certificate and signs it with his private key
    - Every spending of a debt certificate distributes a revocation of that certificate
    - If a token is in the distributed database, it is assumed to be spent
    - Unforgeable digital token? Copy-once?
    - Record that A has paid N dollars to B
    - Blockchain
        - [WP: Blockchain](https://en.wikipedia.org/wiki/Blockchain)
        - "Once recorded, the data in any given block cannot be altered retroactively without the alteration of all subsequent blocks, which requires collusion of the network majority."
    - https://en.wikipedia.org/wiki/Homomorphic_encryption
    - Blockchains are pseudonymous, not anonymous.
        - Is this legit? https://securingtomorrow.mcafee.com/mcafee-labs/staying-anonymous-on-the-blockchain-concerns-and-techniques/
    - Cardano
        - [WP: Recursive InterNetwork Architecture (RINA)](https://en.wikipedia.org/wiki/Recursive_InterNetwork_Architecture_(RINA))
        - formal verification?
        - Haskell programming language
    - [theconversation.com: The blockchain does not eliminate the need for trust](http://theconversation.com/the-blockchain-does-not-eliminate-the-need-for-trust-86481)
        - "The blockchain does not create or eliminate trust. It merely converts trust from one form to another.
        While we previously had to trust financial institutions to verify transactions, with the blockchain we have to trust the technology itself."
        - [hackernoon.com: Bitcoin is not trustless](https://hackernoon.com/bitcoin-is-not-trustless-350ba0060fc9)
    - Security is trust management.
        - Security is minimizing the required trust.
- Gold vs data: move vs copy
    - Gold doesn't need witness to be valuable.
    - Gold can be moved, but not copied.
    Data can't be moved, but can be copied.
    "Moving" data means copying it and deleting the original.
    - Unforgeable and anonymous
        - Gold is unforgeable and anonymous
            - Yes, you can forge gold with nuclear transmutation, but it's not cost-effective
                - so it's practically unforgeable
        - Private key is unforgeable and anonymous
            - Yes, you can forge private key with brute force, but it's not cost-effective
                - so it's practically unforgeable
    - Payment
        - Payment is made by moving the gold
        - Payment is made by moving the private key? But it's duplicated. Data can be copied, but not moved.
    - Double-spending is possible due to the easy-to-copy nature of digital data.
    - What is double-spending?
- Currency is a way of moving debt
    - Bitcoin white paper: [Satoshi Nakamoto, "Bitcoin: A Peer-to-Peer Electronic Cash System"](https://bitcoin.org/bitcoin.pdf)
    - Did SatoNak consider human greed, stupidity, and selfishness?
- Proof of ...
    - proof of work
    - proof of capacity
    - memory-bound functions / memory-hard challenges for spam filtering
- [YouTube: Real Engineering: Why Bitcoin Is Not Working](https://www.youtube.com/watch?v=6q5mUNEEn2c)
- [NUS blockchain research group](https://blockchain-nus.github.io/)
- What is Bitcoin?
    - Need review
        - Bitcoin is rare (and hard to fake) like gold,
        but it's digital in the sense that you can send it quickly over the Internet,
        so bitcoin is digital gold.
        - Bitcoin is not money, but a bitcoin exchange will accept it
        and give you an amount of money they think it's worth.
        You can exchange it with others' cash, goods, or services,
        but only if they accept bitcoin.
        - All the ruckus about bitcoin arises from mixing up
        the concept of
        [medium of exchange](https://en.wikipedia.org/wiki/Medium_of_exchange)
        and
        [unit of account](https://en.wikipedia.org/wiki/Unit_of_account)
        those are two of the
        [functions of money](https://en.wikipedia.org/wiki/Money#Functions).
        - Using bitcoin for payment is comparable to a [hawala](https://en.wikipedia.org/wiki/Hawala).
        - Bitcoin is only useful if people will exchange it with something else.
- [Op Ed: How Tokenization Is Putting Real-World Assets on Blockchains](https://bitcoinmagazine.com/articles/op-ed-how-tokenization-putting-real-world-assets-blockchains/)
