## How should we see this?

Which of these are true about this system?

- an ontology
- a meta-model
- a meta-programming tool
- a program generation tool
- a program derivation tool
- an executable specification language
- an instance of model-driven software engineering
- a Java web framework written in Prolog
- a Java+JavaScript full-stack web framework written in Prolog
- a very rigid, limited, and opinionated framework
- a web application generator
- an enterprise application generator
- a 10x-software-engineer's tool
- a 100x-software-engineer's tool
- a 1000x-software-engineer's tool
- a software template
- a software factory
- a possible future of software engineering
- a very high-level programming language (if Java is high-level)
- a system in which programmers specialize into language designers and language users
instead of into back-ends and front-ends
- a system that captures essential complexity
- a silver bullet, a panacea
- a Prolog alternative to UML
- a way of programming in the language of thought

## Usage

### Basic usage

An example is in [main.pro](main.pro).
Describe your system.
Capture the facts.
Write a model that conforms to our schema.
This is irreducible essential complexity.
This is the minimum amount of information that a human needs to make the system.
This cannot be reduced further,
until we invent telepathy or human-level artificial intelligence.

Translate the model into implementation using our translation rules.

### Intermediate usage

Write your own translation rules.

### Advanced usage

Write a schema.

Design a meta-model.

Design an ontology.

Design a language, especially its semantics.

## Maintenance notes

### Structure overview

[my_sgml_write.pro](my_sgml_write.pro) is monkey-patched SWI-Prolog 7.6.4 library(sgml_write).
Beware of breakage on upgrade.

### Directory structures

[`language`](language/) contains language definitions.

[`schema`](schema/) contains schema definitions.
A Prolog module may _conform_ to a _schema_.
A schema reserves some predicate names and gives meaning to them.
A Prolog module can be thought as an SQL schema.
A Prolog predicate can be thought as an SQL table.
These schemas are not meant to be used with use_module/2.

[`prolog`](prolog/) is about tailoring SWI-Prolog 7.6.4,
mostly about module system, term_expansion/2, and some monkey-patching.
Users should not use_module/2 anything other than [prolog/customization.pro](prolog/customization.pro).