---
title: Modeling all data
permalink: /modeldata.html
date: 2018-08-24 01:31 +0700
---

- Relating ontology, Haskell records, and Java classes
    - We can translate a Haskell record (algebraic data type with exactly one constructor) to a Java final class whose fields are final.
        - One Haskell record field becomes one final Java class field.
    - We want to create a language that can express "ADT B is obtained by adding a Loc field to every constructor of ADT A".
        - `B = Loc * A` (if A is not recursive)
        - This also solves the AST decoration problem.
        Often shows up in making a programming language:
        `Exp = Fix (\ a -> Loc * ExpF a)` where `ExpF a = Con Int | Neg a | Add a a | Mul a a`.
    - We want to express "function 'getName' works with every record that has a 'name' field".
    (Row polymorphism.)
        - `getName : { name : String, ... } -> String`
    - What is a record?
        - A record is a tuple whose components are named.
    - What is a class?
        - [Class-based programming]({% link cbp.md %})
- Prior arts
    - [Data model - Wikipedia](https://en.wikipedia.org/wiki/Data_model)
    - [Data modeling - Wikipedia](https://en.wikipedia.org/wiki/Data_modeling)
    - [Database model - Wikipedia](https://en.wikipedia.org/wiki/Database_model)
- Working with XML
    - Essential editor features
        - Automate closing tag.
        - Editing an opening tag also edits the closing tag.
        - Editing a closing tag also edits the opening tag.
    - Supporting XML Schema Definition (XSD)
        - In principle, from a type defined in an XSD file, we can generate a Haskell module and a Java class.
            - The problem: Finding an XSD editor.
                - IntelliJ IDEA works for me, but I'd be happy if there is a lighter alternative.
                - [XML Schema Editor - Wikipedia](https://en.wikipedia.org/wiki/XML_Schema_Editor#cite_note-1)
        - [W3C XML Schema: DOs and DON'Ts](http://www.kohsuke.org/xmlschema/XMLSchemaDOsAndDONTs.html)
        - [WP:Comparison of XML editors](https://en.wikipedia.org/wiki/Comparison_of_XML_editors)
            - Pick the one with open-source license and schema-aware autocompletion.
                - 2018-08-22
                    - emacs nxml-mode
                    - jEdit XML plugin
                    - QXMLEdit? Can it autocomplete?
                    - XML Notepad
- ontology and programming
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
