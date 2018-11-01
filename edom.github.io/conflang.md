---
title: Designing configuration languages
permalink: /conflang.html
date: 2018-08-31 03:21 +0700
---

# Designing configuration languages

## What is a configuration language?

Configuration language is programming language minus Turing-completeness.

## What is the best configuration language?

2018-08-31:
[Dhall](https://github.com/dhall-lang/dhall-lang) is the pinnacle of configuration languages, in my opinion, as far as I know.

Can a configuration language get any better than Dhall?

How far can we push configuration languages without Turing-completing it?

Dhall isn't the only Turing-incomplete language.
There are also Coq, Lean, Agda, and others.
Can we use these as configuration languages?
Should we?

## Some rants

Write your configuration in Dhall.
You minimize duplication.
It generates YAML/JSON.

Why stop there?
Replace all your YAML, JSON, XML, INI, PROPERTIES, configurations with Dhall.

2018-08-31:
We're considering HashiCorp Terraform.
I think they should use Dhall, or at least learn from Dhall, instead of creating their own
[HCL (HashiCorp Configuration Language)](https://www.terraform.io/docs/configuration/syntax.html).
We have a "Terraform Workaround Engineer" here at work.

Someone has done that: [dhall-terraform](https://github.com/blast-hardcheese/dhall-terraform).
