---
title: Working around Markdown
permalink: /markdown.html
date: 2018-08-21 20:31 +0700
---

## Markdown sucks at mixing indented lists and fenced code blocks.

But that is a corner case.
I can work around it without sacrificing too much.

Workaround: Never indent list after ending a fenced code block.

- Related:
    - [Code block is not properly formatted when placed immediately after a list item - Meta Stack Exchange](https://meta.stackexchange.com/questions/3327/code-block-is-not-properly-formatted-when-placed-immediately-after-a-list-item)
        - ambiguous specification
    - [Fenced code in bullet lists with GitHub-flavoured MarkDown??](https://gist.github.com/clintel/1155906)
    - [Markdown Tutorial - Nested Lists](https://commonmark.org/help/tutorial/10-nestedLists.html)
    - [Why You Shouldn’t Use “Markdown” for Documentation — Eric Holscher - Surfing in Kansas](http://ericholscher.com/blog/2016/mar/15/dont-use-markdown-for-technical-docs/)
        - Disagreements in [Why You Shouldn’t Use “Markdown” for Documentation : programming](https://www.reddit.com/r/programming/comments/4ck2lu/why_you_shouldnt_use_markdown_for_documentation/).

This is an example code (indented by 4 spaces to avoid Markdown breakage):

```
    - List.

    ```
    fenced code block
    ```

        - This should be deeper indented list, but this becomes a code block.
```

(Begin output.)

- List. (Why does Jekyll 3.7.0 insert a `p` tag here?)

```
fenced code block
```

    - This should be deeper indented list, but this becomes a code block.

(End output.)

Pandoc Markdown is content with `\( \)` for delimiting inline mathjax.
GFM (GitHub Flavored Markdown) requires `\\( \\)`.

## GFM inserts table at stupid places.

Example:

```
[foo | bar | baz](https://example.com)
```

Output:

[foo | bar | baz](https://example.com)
