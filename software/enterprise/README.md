# What is this?

I don't know how to describe this.
See [ramble.md](ramble.md)

## Usage

### Basic usage

For basic usage, you can treat this system as a high-level programming language.

An example is in [main.pro](main.pro).
Describe your system.
Capture the facts.
Write a model that conforms to our schema.
This is irreducible essential complexity.
This is the minimum amount of information that a human needs to make the system.
This cannot be reduced further,
until we invent telepathy or human-level artificial intelligence.

Then, translate the model into implementation using our translation rules.

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

[`boot`](boot/) is about tailoring SWI-Prolog 7.6.4,
mostly about module system, term_expansion/2, and some monkey-patching.
These files should be loaded with consult/1, not with use_module/2.

[`library`](library/) contains schemas and translations.

A Prolog module may _conform_ to a _schema_.
A schema reserves some predicate names and gives meaning to them.
A Prolog module can be thought as an SQL schema.
A Prolog predicate can be thought as an SQL table.
