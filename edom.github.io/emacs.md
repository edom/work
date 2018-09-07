---
title: Using Emacs
permalink: /emacs.html
date: 2018-09-07 16:11 +0700
---

- TOC
{:toc}

## Install Ubuntu 14.04

### Install Emacs

On 2018-09-07, the command
`sudo apt-get install emacs magit org-mode`
on Ubuntu 14.04 will install:

- emacs 24.3.1
- magit 1.2.0
- org-mode 8.2.4

Org Mode is the gateway drug to Emacs.

### Fixing misbehaviors

If Emacs is misbehaving (and if you are an Emacs beginner who have no idea why), try nuking the file `~/.emacs` and the directory `~/.emacs.d`.
But you will lose all customizations you have made.

If you are an advanced Emacs user,
you may want to commit your modifications to your personal Git repository,
and make `~/.emacs` a symbolic link.
Unfortunately, we don't always want this because
that file may contain sensitive data (such as IRC passwords).

## Basics

### Key notation

`C-a` means hold Ctrl and press the A key.

`M-a` means hold Meta (Alt) and press the A key.

`S-a` means hold Shift and press the A key.

`RET` means press the Return/Enter key.

`LEFT` means press the Left Arrow key.

`BKSP` means the Backspace key.
Emacs calls this key `DEL`.

`SPC` means the Space key.

`C-a C-b` means `C-a` and then `C-b`.

`M-x help` means press `M-x` and then type `help` (and press Enter if necessary).

### Common keys

To exit Emacs, `C-x C-c`.

To cancel a long-running action, or exit a prompt, press `C-g`.

To start/open/visit a file, `C-x C-f`.
The file doesn't have to exist.

To autocomplete (where a file name or command name is expected), press `TAB`.

### Customizing the theme/color

`M-x customize-themes`.

### The Emacs tutorial that comes with Emacs

Follow the emacs tutorial `C-h t`.

Read the emacs manual `C-h r`.

## Using buffers and windows

- `C-x C-f` opens a file or directory into a buffer
- `C-x C-b` lists buffers in the other window
- `C-x 1` delete other windows (all windows except the focused one)
- `C-x C-LEFT` goes to previous buffer
- `C-x C-RIGHT` goes to next buffer
- `C-x k` kills a buffer
- `C-x s` saves some buffers
- `C-x b` switches buffers

## Setting a desktop/session so that emacs continues where you left off

## Searching

- `C-s` forward incremental search
- `C-r` reverse incremental search
- While in incremental search prompt:
    - `C-s` go to next occurrence
    - `C-r` go to previous occurrence

## Basic motions

### Moving by a character or a line

- horizontally/characterwise
    - `C-b` moves the cursor back to the previous character
    - `C-f` moves the cursor forward to the next character
    - `C-a` moves the cursor to the beginning of line
    - `C-e` moves the cursor to the ending of line
- vertically/linewise
    - `C-n` moves the cursor down to the next line
    - `C-p` moves the cursor up to the previous line

### Moving by a screen

- `M-v` goes up a screen
- `C-v` goes down a screen
- `C-M-v` goes down a screen in the other window
- `C-M-S-v` goes up a screen in the other window

(Emacs call scroll-*up*-command for what I think as going *down* a screen.
Emacs thinks about moving the text.
I think about moving the viewport.)

## Using dired to navigate the file system

Input `C-x C-f`, input a directory path, and press Enter.
Emacs opens Dired (directory edit) mode.

- Navigating:
    - `BKSP` moves the cursor up one item.
    - `SPC` moves the cursor down one item.
    - `l` (small L) refreshes the display.
- Opening:
    - `RET` opens the item in the current window.
    If it's a directory, emacs opens another dired.
    If it's a file, emacs opens the editor.
    - `o` (small O) opens the item in the other window.

Use `M-x describe-mode` to get some help.

## Getting started with org-mode

### How do people use org-mode?

- [Org Mode - Organize Your Life In Plain Text!](http://doc.norang.ca/org-mode.html)

### Entering org mode

Visit a file with `.org` extension.
Alternatively, `M-x org-mode`.

### Reading the manual

Read the manual with `M-x org-info`.

A shorter HTML version is available online: [the compact org-mode guide](https://orgmode.org/guide/).
However, I find that it's more convenient to browse the info document in emacs.

#### Navigating an info document

- `q` quits the info viewer
- `RET` follows the link under cursor
- `TAB` moves the cursor to the next link
- `S-TAB` moves the cursor to the previous link
- `l` (small L) goes back to the previous page in your browsing history
- `u` goes up to the parent page (of the current page)
- `n` goes to the next page
- `p` goes to the previous page
- `h` brings up help about how to use the info viewer

### Inserting lists

Begin the line with a hyphen (`-`).

`M-RET` to enter the next item.

`RET` to end the list.

`TAB` to indent the current item one level inward.

`S-TAB` to unindent the current item one level outward.

## Unanswered questions

- What is the Emacs equivalent of VSCode Ctrl+P (open file fuzzy search / approximate string matching / subsequence matching)?

## For vi users

- [Emacs-Vi correspondence table]({% link emacsvi.md %})

### Evil mode? Vile? VIPER?

How do I install evil-mode (vi keybindings for emacs)?
Should I?
It's not on GNU ELPA.

- [package - Do I still need GNU ELPA if I have MELPA? - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/10500/do-i-still-need-gnu-elpa-if-i-have-melpa)
- [Is evil-mode too evil? : emacs](https://www.reddit.com/r/emacs/comments/6ej18a/is_evilmode_too_evil/)

There is a "vile" (vi-like emacs) package on Ubuntu 14.04.

Emacs 24 FAQ recommends VIPER for people who want vim's `.` command.

## Going deeper

- Read the emacs manual in emacs.
- magit: [It's Magit! A Git Porcelain inside Emacs](https://magit.vc/)
    - vs git-el?
- erc: IRC with emacs
- opening PDF with emacs
- browsing the Internet with emacs
- icicles: "emacs library that enhances minibuffer/input completion"
- programming: paredit, haskell-mode, golang-mode, etc.

## Going even deeper

- Write an Emacs Lisp program
- Compare Emacs Lisp and Vim Script

## Resources

- [Vivek Haldar — The levels of Emacs proficiency](http://blog.vivekhaldar.com/post/3996068979/the-levels-of-emacs-proficiency)
- [Org mode for Emacs – Your Life in Plain Text](https://orgmode.org/)
- [Xah Lee's Emacs tutorial](http://ergoemacs.org/emacs/emacs.html)

## Other things I haven't tried

### Emacs stuffs that may be related to org-mode

- "BHL is an Emacs mode that enables you to convert text files into HTML, LaTeX and SGML files."
- "Howm(Hitori Otegaru Wiki Modoki) is a note-taking tool on Emacs."
- emacs-wiki
- mhc: schedule management tool for emacsen
- muse-el: Author and publish projects using Wiki-like markup
- smartdoc-elisp: emacs mode for smartdoc
- sisu: documents - structuring, publishing in multiple formats and search

### Other editors

- [Diakonos - a linux editor for the masses](http://diakonos.pist0s.ca/)
- Spacemacs
- non-free
    - [Sublime Text - A sophisticated text editor for code, markup and prose](https://www.sublimetext.com/)
- TeX/LaTeX
    - GNU TeXmacs
    - WhizzyTeX
- XEmacs? emacs-lucid?
    - [WP:XEmacs history](https://en.wikipedia.org/wiki/XEmacs#History)
    - [Xah Lee: My Experience of Emacs vs XEmacs](http://ergoemacs.org/emacs/emacs_vs_xemacs.html)
        - [Ben Wing: GNU Emacs and XEmacs Schism](http://ergoemacs.org/emacs/gnu_emacs_xemacs_schism_Ben_Wing.html)

## what

- remembrance-agent: Emacs mode to help find relevant texts
- twittering-mode: Twitter client for Emacs
- [Famous Programers with Repetitive Strain Injury](http://ergoemacs.org/emacs/emacs_hand_pain_celebrity.html)
