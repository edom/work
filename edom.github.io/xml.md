---
title: Using XML
permalink: /xml.html
date: 2018-08-30 00:28 +0700
---

* TOC
{:toc}

## Finding the editor

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
- Is there any way to avoid memorizing and typing `xmlns:xsi`?
- Are there schema-aware/schema-oriented XML editors?

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
