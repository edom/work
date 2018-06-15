{- |

Write metaprograms with Haskell as the metalanguage.

A /metaprogram/ is a program that manipulate other programs, possibly in other languages.

* Examples

    * "Meta.Example": an example application

    * "Meta.ExampleTables": its tables

* Relational databases

    * "Meta.Relat": a subset of relational databases, mostly SQL databases

    * "Meta.SqlCon": connect to SQL databases using HDBC

* Intermediate programming languages

    * "Meta.IntCbp": intermediate class-based programming language

        * practically all common programming languages

        * similar to C, C++, Java, C#, Python, and Go

* Programming languages

    * Haskell

        * "Meta.Hs": a subset of Haskell, similar to Template Haskell and Strathclyde Haskell Enhancement, but not hygienic and not typechecked

    * Java

        * "Meta.Java": a subset of Java

        * "Meta.JavaServlet": generate Java servlet

        * "Meta.Maven": a subset of Maven (dependency management, build system, and central repository, for Java)

        * "Meta.MavenDep": Maven dependency

    * TypeScript

        * "Meta.Tys": a subset of TypeScript

* Web

    * "Meta.Web": a website description language

* Support

    * Formatting

        * "Meta.WrapM": layout text into indented length-limited lines

    * Transput (input and output)

        * "Meta.File": write files

    * "Meta.Prop"

        * enhance Haskell usability

        * work around Haskell limitation (name clash inside module)

    * "Meta.Xml": write XML (Extensible Markup Language) documents

    * "Meta.Readme": overview, this file

    * Internal: Do not use

        * "Meta.Wrap": internals of "Meta.WrapM"

        * "Meta.Hs" internals

                * "Meta.HsCon": ADT (algebraic data type) constructors

                * "Meta.HsDat": ADTs

                * "Meta.HsExp": expressions

                * "Meta.HsMod": modules

                * "Meta.HsPat": patterns

                * "Meta.HsRender": render Haskell source files

                * "Meta.HsType": types

        * "Meta.Java" internals

                * "Meta.JavaSta": statements

                * "Meta.JavaType": types

                * "Meta.JavaRender": render Java source files

-}
module Meta.Readme where
