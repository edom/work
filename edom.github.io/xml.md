---
title: Using XML
permalink: /xml.html
date: 2018-08-30 00:28 +0700
---

* TOC
{:toc}

## Finding the editor

List of editors:

- I have used
    - Vim
    - IntelliJ IDEA
- I don't know
    - emacs nxml-mode
    - jEdit XML plugin
    - QXMLEdit? Can it autocomplete?
    - XML Notepad

Sources:

- [WP:XML Schema Editor](https://en.wikipedia.org/wiki/XML_Schema_Editor#cite_note-1)
- [WP:Comparison of XML editors](https://en.wikipedia.org/wiki/Comparison_of_XML_editors)

IntelliJ IDEA works fine, but I'd be happy if there is a lighter alternative.

My criteria:

- open-source
- schema-aware, supports XML Schema Definition (XSD)
- autocompletion
    - 2018-08-22

Nice-to-have features:

- Automate closing tag.
- Editing an opening tag also edits the closing tag.
- Editing a closing tag also edits the opening tag.
- Any way to avoid memorizing and typing `xmlns:xsi`.

Vim can semi-automate XML closing tag using Ctrl+P.

## Using schemas

In principle, from a type defined in an XSD file, we can generate a Haskell module and a Java class.

- [W3C XML Schema: DOs and DON'Ts](http://www.kohsuke.org/xmlschema/XMLSchemaDOsAndDONTs.html)

## Toward markup language agnosticity

What is common between XML, JSON, and YAML?

Can we map (interconvert) between XML and YAML?
Approximate data model:

```haskell
type Key = String
type Atr = (Key, String)

data Xml
    = XText String
    | XElem [Atr] Name [Xml]

data Json
    = JText String
    | JMap [(Key, Json)]
    | JList [Json]

xj :: Xml -> Json

jx :: Json -> Xml
```

Can we use XML Schema to validate a JSON/YAML document?

We can map a subset of XML to YAML using [YAXML, the (draft) XML Binding for YAML](http://yaml.org/xml).

- [In choosing between XML and YAML, what are advantages and more natural problem domains for each one? - Quora](https://www.quora.com/In-choosing-between-XML-and-YAML-what-are-advantages-and-more-natural-problem-domains-for-each-one)
