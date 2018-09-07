---
title: Using Emacs
permalink: /emacs.html
date: 2018-09-07 16:11 +0700
---

- TOC
{:toc}

## Install Ubuntu 14.04

### Install Emacs

- emacs 24
- org-mode 8.2.4

```
sudo apt-get install emacs org-mode
```

Org Mode is the gateway drug to Emacs.

### Fixing misbehaviors

If Emacs is misbehaving (and if you are an Emacs beginner who have no idea why), try nuking the file `~/.emacs` and the directory `~/.emacs.d`.
But you will lose all customizations you have made.

If you are an advanced Emacs user,
you may want to commit your modifications to your personal Git repository,
and make `~/.emacs` and `~/.emacs.d` symbolic links.

## Basics

### Key notation

`C-a` means hold Ctrl and press the A key.

`M-a` means hold Meta (Alt) and press the A key.

`S-a` means hold Shift and press the A key.

`RET` means press the Return/Enter key.

`C-a C-b` means `C-a` and then `C-b`.

### For vi users

How do I install evil-mode (vi keybindings for emacs)?

### Common keys

To exit Emacs, `C-x C-c`.

To cancel a long-running action, or exit a prompt, press `C-g`.

To start/open/visit a file, `C-x C-f`.
The file doesn't have to exist.

To autocomplete (where a file name or command name is expected), press `TAB`.

### Customizing the theme/color

`M-x customize-themes`.

## Basic motions

### Moving by a character or a line

- horizontally/characterwise
    - `C-b` moves the cursor back to the previous character
    - `C-f` moves the cursor forward to the next character
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

### Moving through buffers

- `C-x C-LEFT` goes to previous buffer
- `C-x C-RIGHT` goes to next buffer

## Getting started with org-mode

### Reading the manual

Read the manual with `M-x org-info`.

A shorter HTML version is available online: [the compact org-mode guide](https://orgmode.org/guide/).
However, it's more convenient to browse .

#### Navigating an info document

- `q` quits the info viewer
- `RET` follows the link under cursor
- `TAB` moves the cursor to the next link
- `S-TAB` moves the cursor to the previous link
- `l` (lowercase L) goes back to the previous page in your browsing history
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

## Resources

- [Vivek Haldar — The levels of Emacs proficiency](http://blog.vivekhaldar.com/post/3996068979/the-levels-of-emacs-proficiency)
- [Org mode for Emacs – Your Life in Plain Text](https://orgmode.org/)
- [Xah Lee's Emacs tutorial](http://ergoemacs.org/emacs/emacs.html)
