---
title: Modeling all data
permalink: /modeldata.html
date: 2018-08-24 01:31 +0700
---

We are concerned about two things: what we need to store, and how we store it.

Business process automation begins with data modeling.

An *entity* is something that has an identity:
an entity persists through time.
In relational databases, an entity is a row with a primary key;
two entities are assumed to be identical iff their primary keys are equal.

We can store data as array of fixed-size records on disk.

Suppose we are building an online shop.
We sell disparate things such as houses, cars, and phones.
We want each item type to have tailored search
because people have different concerns:

- Someone buying a used car is concerned about mileage.
- Someone buying a house is concerned about location.
- Someone buying a phone is concerned about RAM size.

Every buyer has his *model* of the thing he wants to buy.
Aligning the website's model to the buyer's model should increase sales.
A model is what we care.

There are several models of a car:

- Visual model: set of 3D triangles.
- Engineering attributes model: engine size, crash safety rating.
- Economics attributes model: fuel economy, price.

Every model captures an aspect and ignores everything else.

What is a car? All these answers are partially true:

- A car is a vehicle with four wheels.
- A car is a tuple (manufacture-year, sale-price).
- A car is an engine container.
- A car is a set of triangles and a tuple (x,y,z,vx,vy,vz).

There are no true or false answers.
There are only useful or useless answers.

The identity problem also affects data modeling.

We combine the models that have the data that our application needs.

We assume primitive data types such as unsigned/two's-complement-signed 8/16/32/64-bit integer.

Row polymorphism should help write enterprise software because most data in enterprise software are records.

## Relating ontology, Haskell records, and Java classes

A record is a tuple whose components are named.

What is a class? Class-based programming?

We can translate a Haskell record (algebraic data type with exactly one constructor) to a Java final class whose fields are final.
One Haskell record field becomes one final Java class field.

- We want to create a language that can express "ADT B is obtained by adding a Loc field to every constructor of ADT A".
    - `B = Loc * A` (if A is not recursive)
    - This also solves the AST decoration problem.
    Often shows up in making a programming language:
    `Exp = Fix (\ a -> Loc * ExpF a)` where `ExpF a = Con Int | Neg a | Add a a | Mul a a`.
- We want to express "function 'getName' works with every record that has a 'name' field".
(Row polymorphism.)
    - `getName : { name : String | _ } -> String`
- Prior arts
    - [Data model - Wikipedia](https://en.wikipedia.org/wiki/Data_model)
    - [Data modeling - Wikipedia](https://en.wikipedia.org/wiki/Data_modeling)
    - [Database model - Wikipedia](https://en.wikipedia.org/wiki/Database_model)

- ontology and programming
    - [Stanford Protege Wiki](https://protegewiki.stanford.edu/wiki/Main_Page) has several tutorials about writing ontologies.
    - "ontoprog: Ontology-based Programming: Extended Semantics for OOP languages", [github](https://github.com/andreasBihlmaier/ontoprog)
    - 2012, article, "Modeling the Knowledge Domain of the Java Programming Language as an Ontology", [pdf](http://eeyem.eap.gr/wp-content/uploads/2017/06/11_ICWL2012.pdf)
    - 2007, article, "Towards a programming language ontology", [pdf](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.82.194&rep=rep1&type=pdf)
- REST ontology
    - REST forces hierarchical ontology.
    - https://server/property/101/name -> "foo"
        - "101" is an instance of "property" class.
        - "property" is a class in "server" namespace.
        - Attribute "name" of "101" has value "foo".
    - https://server/property/101 -> {"name": "foo", "location": "somewhere"}
        - "101" is an instance of "property" class.
    - https://server/property/101/stock/level/day -> 10
        - means that property 101 can last 10 days
        - A/B mean A has B?
        - A/B mean B is an instance of A?
    - [REST is OVER! - Literate Programming](http://blog.steveklabnik.com/posts/2012-02-23-rest-is-over)
        - REST has been renamed to "Hypermedia API".
- https://en.wikipedia.org/wiki/Semantic_data_model
