# Procedure Action Language (PAL): Design

## Design principle: Match the end-user's mental model

The principle of PAL is to think about how the _end-user_ would describe what the system does.
A PAL programmer should only care about what an end-user cares about.
For example, if the end-user thinks that the system stores data,
then PAL should have a primitive about data storage.

If the end-user does not care about something, then the PAL programmer should also not care about it.
For example, the end-user does not care about how the system actually stores data;
what the end-user cares about is that the same stored data is retrievable later.
Thus PAL should delegate the meaning of "to store data" to the translators,
but with the semantics that retrieving a stored data should exactly reproduce the stored data.
Thus, PAL constrains but does not determine the meaning of its primitives.

The end-users do not care about processor register sizes and integer overflows.
They assume that all numbers are as large as they need to be.
They only care about whether the system meet their requirements.
They do not care about the implementation details.

Thus a PAL programmer can be thought as a end-user that knows some formalism.

The programmer should be able to use a database as if it were just a global variable.

## Design issues

Which paradigm is the most convenient for business logic: procedural, functional, logic, data-flow, what?
    - Should we use relational algebra?
        - Pro: Simplifies translation to SQL.
        - Con: No transitive closure, unless we bolt on a transitive-closure operator,
        but then the mapping to SQL would be complicated.

What is the difference between a type and a check?
Aren't types for checking?

Should it be called a "procedure"? What should it be called?
    - task, user task
    - process, business process
    - activity
    - interaction
    - transaction, atomic interaction
    - function, functionality

## Things to consider

- Datalog
- 1995, Hillebrand, Kanellakis, & Mairson, "Database Query Languages Embedded in the Typed Lambda Calculus"

Lambda-calculus with bells and whistles such as relational algebra expressions?
