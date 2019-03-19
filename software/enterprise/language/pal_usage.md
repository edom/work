# Procedure Action Language (PAL): Usage

## Procedure

A _procedure_ has _inputs_ and _checks_.

- If any check fails, the action does not run.
- The system asks the user for the inputs.
- The action runs after all inputs are ready.
- A procedure does not have _state_.
A procedure belongs to a system that may have state.

The word "procedure" here does not imply an imperative programming style.

## Example?

(Explain?)

```
procedure_definition(
    sum_and_product,                % identifier
    inputs-[x:integer, y:integer]
    , output-record([
            sum-(x + y)
          , product-(x * y)
        ])                          % assume type inference
    , [                             % options
        checks-[
            0 =< x, x < 100
          , 0 =< y, y < 100
        ]
    ]
).
```

Another example?

```
auto_crud(employee).
auto_crud(department).

procedure_definition(add-Class,
    [x:Class],
    insert(Class, x)
) :- auto_crud(Class).
```

## Types and values

These mathematical objects are commonly used in business:

    - numbers, up to real numbers
    - character strings (unformatted text)
    - dates and times
    - records composed of those objects

These are the types:

- Number types

    - natural
    0, 1, 2, ...

    A natural number is either zero or a successor of a natural number.

    - integer
    An integer is a natural number or its negative (additive inverses).

    - rational
    A rational number is the ratio of two integers.

    - real
    A real number is the limit of a series of rational numbers.

- Character string types

    - string
    A "string" by itself means a character string.

    A character string is a sequence of characters.

    - character
    What?

- Collection types

    - set
    Unordered and unique.

    - set(ElementType)
    Homogeneous set.

    - vector(ComponentType)
    Similar names: homogeneous tuple, fixed-count sequence.

- Temporal types

    - temporal(Params)
    A temporal value is an answer to the question of when something happens.

    Params is a list.

        - instant
        Seconds since Unix epoch?

        - date
        Indicates that the temporal type has a date component.

        - time
        Indicates that the temporal type has a time component.

        - zone
        Indicates that the temporal type has a zone component.

## Expressions

### Relational expressions

Each Type implies a set.

- insert(Type, Object)
Store.
Generate identifier.
Similar terms: save, remember, assert.

- delete(Type, QueryExp)
Similar terms: forget, remove, retract.

- select(TableExp)
Similar terms: search, lookup, get.

TableExp is a table expression, a relational-algebraic expression.

The meaning of a table expression is a table.

A table has columns and rows.

TableExp:

    - table(Name)
    This means all rows of the table named Name, as defined by table/1.

    - satisfy(CondExp, TableExp)
    Mostly relational algebra

    - project(Column, TableExp)
    Project, or project-and-rename.

    - order_by(OrderExp, TableExp)

    - product(TableExpList)
    - product(TableExp, TableExp)
    - TableExp * TableExp
    Cartesian product.

    - limit(Limit,TableExp)

### State expressions

- state(Name)
Get the named state.
Name is a ground term.

- state(Name) := Exp
Mutate the named state.

### Boolean expressions

- A < B
- A = B
- A > B
- A =< B
- A >= B
Compare two things.
    - Compare two numbers.
    - Compare two strings according to an implementation-dependepent collation.

### Numerical expressions

- A + B
- A - B
- A * B
Each of A and B is an expression.
These are mathematical operations of unlimited numbers,
not modular arithmetics. not two's complement, no overflow.
If an implementation uses fixed-precision arithmetics,
then it must fail loudly if it cannot represent the result.
If an implementation uses arbitrary-precision arithmetics,
then it must fail loudly if it runs out of space.

- A / B
Division of real numbers.
The divisor B must be _positive_.
If the divisor is negative, the implementation must fail.
It is possible, but hard, to make sense of _negative divisors_.
Thus we ban them.
We believe they should not happen in business.

- abs(NumExp)
The absolute value of the real number.

- ceil(NumExp)
Round a real number up to the nearest more-positive integer.

- floor(NumExp)
Round a real number down to the nearest more-negative integer.

### Character-string expressions

- append(A,B)
Concatenates two character strings.

### Conditional expressions

- if(Condition, TrueExp, FalseExp)
If Condition is true, reduce to TrueExp.
If Condition is false, reduce to FalseExp.

### Binding expressions

    - let(Name, Exp, Body)
    Replace every free occurrence of Name in Body.

### Compound expressions

    - A, B
    Ordered evaluation.

    - A; B
    Unordered evaluation.

## Operational semantics

A procedure must execute _atomically_ with respect to the application state.

Execution of different procedures must not interfere with each other.
They must be isolated from each other.

If an implementation uses optimistic locking,
side-effects may be repeated.

Java implementation may use synchronized blocks.

A procedure may _call_ another procedure.
