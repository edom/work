{- |

Write metaprograms with Haskell as the metalanguage.

A /metaprogram/ is a program that manipulate other programs, possibly in other languages.

To use, import "Meta.User".

-}
module Meta.Readme where

-- * Internals

{- $

These are not part of the public API.
Do not use.

* Examples

    * "Meta.Example": an example application

    * "Meta.ExampleTables": its tables

* Data

    * "Meta.Data": data description language

        * "Meta.Data_internal": its internals

    * "Meta.SqlCon": connect to SQL databases using HDBC

    * "Meta.SqlType": SQL types

    * "Meta.SqlSyn": SQL abstract syntax

* Programming languages

    * Haskell

        * "Meta.Hs": a subset of Haskell, similar to Template Haskell and Strathclyde Haskell Enhancement, but not hygienic and not typechecked

            * "Meta.HsDat": ADTs

                * "Meta.HsCon": ADT (algebraic data type) constructors

            * "Meta.HsExp": expressions

            * "Meta.HsMod": modules

            * "Meta.HsPat": patterns

            * "Meta.HsRecord": records

            * "Meta.HsRender": render Haskell source files

            * "Meta.HsType": types

    * Java

        * "Meta.Java": a subset of Java

            * "Meta.JavaRender": render Java source files

            * "Meta.JavaSta": statements

            * "Meta.JavaType": types

            * "Meta.JavaWebApp": Java web application

                * "Meta.JavaServlet": Java servlet

        * "Meta.Maven": a subset of Maven (dependency management, build system, and central repository, for Java)

            * "Meta.MavenCmd": Maven command-line wrapper

            * "Meta.MavenDep": Maven dependency

    * TypeScript

        * "Meta.Tys": a subset of TypeScript

    * Intermediate languages

        * "Meta.Cbp": intermediate class-based programming language

            * practically all common programming languages

            * similar to C, C++, Java, C#, Python, and Go

            * "Meta.Cbp_internal": its internals

        * "Meta.Lambda": lambda calculus

        * "Meta.Cal": calculator expression language:
        constant expression, arithmetic expression, boolean expression

* Web

    * "Meta.Web": a website description language

    * "Meta.Html": HTML

* Support

    * Formatting

        * "Meta.WrapM": layout text into indented length-limited lines

            * "Meta.Wrap": internals of "Meta.WrapM"

    * Transput (input and output)

        * "Meta.File": write files

    * "Meta.Type": conversion between type systems

    * "Meta.Prop"

        * enhance Haskell usability

        * work around Haskell limitation (name clash inside module)

    * "Meta.Xml": write XML (Extensible Markup Language) documents

    * "Meta.Readme": overview, this file

    * "Meta.Test": tests for this library

-}
