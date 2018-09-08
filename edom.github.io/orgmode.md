---
title: Using Org Mode
permalink: /orgmode.html
date: 2018-09-07 16:11 +0700
---

- TOC
{:toc}

## Entering org mode

Visit a file with `.org` extension.
Alternatively, `M-x org-mode`.

## Reading the manual

Read the manual with `M-x org-info`.
However, on Ubuntu 14.04, there is a catch:
`M-x org-version` says 8.2.4,
but emacs comes with its own org-mode 7.9.3f documentation in `/usr/share/info/emacs-24` that precedes `/usr/share/info`.
Use `C-u C-h i /usr/share/info/org.gz RET` to open the correct documentation.

There is a shorter HTML version online: [the compact org-mode guide](https://orgmode.org/guide/),
but it's more convenient to browse the info document in emacs.

### Navigating an info document

- `q` quits the info viewer
- `RET` follows the link under cursor
- `TAB` moves the cursor to the next link
- `S-TAB` moves the cursor to the previous link
- `l` (small L) goes back to the previous page in your browsing history
- `u` goes up to the parent page (of the current page)
- `n` goes to the next page
- `p` goes to the previous page
- `h` brings up help about how to use the info viewer

## Inserting, editing, and moving headers and subtrees

Begin the line with an asterisk (`*`).

- moving subtrees
    - `M-UP` move subtree up
    - `M-DOWN` move subtree down
- promoting and demoting
    - `M-LEFT` promote header one level shallower
    - `M-RIGHT` demote header one level deeper
    - `M-S-LEFT` promote subtree one level shallower
    - `M-S-RIGHT` demote subtree one level deeper
- inserting dates

## TODOs

- `S-LEFT` / `S-RIGHT` cycle item TODO state
- `S-M-RET` insert TODO below current item
- `S-UP` / `S-DOWN` change item priority (see "TODO Items > Priorities" in manual)

## Agenda

An agenda summarizes/aggregates org files.
See "Agenda Views > Agenda Files" in org mode manual.

## Navigating an org file

- cycling visibility
    - `TAB` cycle subtree visibility
    - `S-TAB` cycle global visibility
    - See also Org Mode manual "Visibility cycling".
- moving around
    - `C-c C-n` next heading
    - `C-c C-p` previous heading
    - `C-c C-f` next heading same level
    - `C-c C-b` previous heading same level
    - `C-c C-u` backward to higher level heading
    - `C-c C-j` jump/goto

## Inserting lists

Begin the line with a hyphen (`-`).

`M-RET` to enter the next item.

`RET` to end the list.

`TAB` to indent the current item one level inward.

`S-TAB` to unindent the current item one level outward.

## Exporting to HTML for use with Jekyll

I have already had a wiki in Jekyll.

- `C-c C-e h h` export thisfile.org to thisfile.html; silently overwrite the output
- `C-c C-e h H` export to buffer; don't create file

The official tutorial [Using org to Blog with Jekyll](https://orgmode.org/worg/org-tutorials/org-jekyll.html) isn't good.
I want Emacs to transform in-buffer setting to front matter.
I don't want BEGIN_HTML.
I want proper metadata mapping.

## Going deeper

- [Creating Gantt charts by Exporting to TaskJuggler](https://orgmode.org/worg/org-tutorials/org-taskjuggler.html)
- Learning org-mode from others
    - [Org Mode - Organize Your Life In Plain Text!](http://doc.norang.ca/org-mode.html)
- Emacs stuffs, that may be similar to org-mode, that I haven't tried
    - "BHL is an Emacs mode that enables you to convert text files into HTML, LaTeX and SGML files."
    - "Howm(Hitori Otegaru Wiki Modoki) is a note-taking tool on Emacs."
    - emacs-wiki
    - mhc: schedule management tool for emacsen
    - muse-el: Author and publish projects using Wiki-like markup
    - smartdoc-elisp: emacs mode for smartdoc
    - sisu: documents - structuring, publishing in multiple formats and search
