## What is this?

I am still figuring out what this is at [https://edom.github.io/enterprise.html](https://edom.github.io/enterprise.html).

I don't know how to describe this.
See also [ramble.md](ramble.md).

## How to read this

Open the folder in Visual Studio Code.

Use Ctrl+P to go to a file.

Start from this file (README.md).

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

We plan to have this command line:

```
swipl -s main.pro ModelFile
```

ModelFile is a Prolog source file.

### Intermediate usage

Write your own translation rules.

To get the stack trace of exceptions thrown from directives,
tun this program with `DEBUG=1` environment variable:

```
# In bash:
DEBUG=1 swipl -l main.pro ARGS...
```

The `-l` option loads the file but skips initialization goals.
The `-s` option loads the file and runs initialization goals.

Run the `main/0` goal to run the program.

### Advanced usage

Write a schema.

Design a meta-model.

Design an ontology.

Design a language, especially its semantics.

## Maintenance notes

### Program start and architecture

First, some files in boot must be loaded (into the `user` module?).
The library files assume some predicates such as throw_error/1
and consult_unregistered_into_module/2.

Then, the components are loaded and linked.
A Prolog _component_ has a source file, plugs, and sockets.
(???)

Each component may be instantiated into a Prolog module.
Each component may be instantiated many times into several such modules.

How to declare connections:
    - schema_connection/2 in main_schema.pro

### Directory structures

[`library`](library/) contains schemas and translations.

A Prolog module may _conform_ to a _schema_.
A schema reserves some predicate names and gives meaning to them.
A Prolog module can be thought as an SQL schema.
A Prolog predicate can be thought as an SQL table.