# What is this?

This is a scratchpad.
It has many mistakes and speculations.
It will get messier as I learn more.

## How do I build this?

You need pdflatex (and maybe some more other stuffs).

```
./make
```

## What is all this XML stuff?

This is a failed attempt at creating an authoring system.

What I was trying to do:
In 2017, I was looking for an authoring format.
I was using Jekyll Markdown, and GFM math markup sucked.
I wanted to write everything in XML, and write XSLT and XQuery to transform the XML into a HTML website.
I shopped around, and saw DITA and DocBook, but none of them seems satisfactory about math markup, so I tried to design my own XML.
I used IntelliJ IDEA to edit XML, XSD, XSLT, and XQuery.

It seemed like a good idea.
I even bragged to my friends that I wrote everything in XML.
In hindsight, it was the worst idea I ever had.
IntelliJ IDEA is an overkill for XML editing,
and XML requires that ampersands and less-than signs be escaped.
Escaping hurts me a lot when I'm writing an inline LaTeX fragment in XML.

I sank a considerable amount of effort into this shit.
My 2018 self murderously hates my 2017 self for this XML ordeal.
I wish I had known org-mode earlier.

Lessons learned:
- Reuse existing solutions.
- Empathize with future self.
- Don't settle for anything worse than org-mode + pandoc + make.
Never author these in XML by hand:
a book, a wiki, a knowledge base, a note, a website, or any a significant amount of writing.
Prefer a lightweight markup with good LaTeX support and good tool support.

## Legal stuff

Copyright 2017 Erik Dominikus
