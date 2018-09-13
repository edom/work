---
title: Using Emacs
permalink: /emacs.html
date: 2018-09-07 16:11 +0700
---

- TOC
{:toc}

## Some convincement and indoctrination

- [Emacs guided tour](https://www.gnu.org/software/emacs/tour/) is a showcase.
Perhaps there you will find a motivation to use emacs.

## Note about key notation

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

## Installing Emacs

On 2018-09-07, the command
`sudo apt-get install emacs magit org-mode org-mode-doc`
on Ubuntu 14.04 will install:

- emacs 24.3.1
- magit 1.2.0
- org-mode 8.2.4

Org Mode is the gateway drug to Emacs.

If you want Emacs 26 on Ubuntu 14.04, try [How to Build Emacs on Linux](http://ergoemacs.org/emacs/building_emacs_on_linux.html).

## Launching Emacs

Launch emacs from your terminal in your working directory.
Use `emacs -fs` to start emacs fullscreen.
Use `emacs -mm` to start emacs maximized.

## Configuring Emacs

Save a desktop/session so that emacs continues where you left off:
`M-x desktop-save`.
See also "Saving Emacs Sessions" in the Emacs manual.
Unfortunately Emacs 24.3 doesn't save the frames and windows.

Set up your emacs initialization file `~/.emacs.d/init.el`.
Make sure that the file `~/.emacs` does not exist;
otherwise Emacs won't read `~/.emacs.d/init.el`.
Copy others' emacs init file, or use pre-configured Emacs derivatives such as Spacemacs.
You may want to commit your modifications to your personal Git repository,
and make `~/.emacs.d` a symbolic link.
Make sure that it doesn't contain sensitive data (such as IRC passwords).

If Emacs is misbehaving (and you have no idea why), try nuking the file `~/.emacs` and the directory `~/.emacs.d`.
But you will lose all your customizations.

## Basics

### Common keys

To exit Emacs, `C-x C-c`.

To cancel a long-running action, or exit a prompt, press `C-g`.

To find/start/open/visit a file, `C-x C-f`.
The file doesn't have to exist.

To open a file in the other window, `C-x 4 C-f`.

To autocomplete (where a file name or command name is expected), press `TAB`.

To start a shell, `M-x shell`.

### Using buffers and windows

- buffers
    - `C-x C-f` opens a file or directory into a buffer
    - `C-x C-b` lists buffers in the other window
    - `C-x C-LEFT` goes to previous buffer
    - `C-x C-RIGHT` goes to next buffer
    - `C-x k` kills a buffer
    - `C-x s` saves some buffers
    - `C-x b` switches to another buffer, or opens a new buffer
        - `C-x 4 b` the same, but in the other window
- windows
    - `C-x 1` delete other windows (all windows except the focused one)
    ` `C-x o` switches to another window / the other window
    - `C-x 2` split window below
    - `C-x 3` split window right
- `C-x 4 0` kill-buffer-and-window

### Getting help

- `C-h ?` help for help
- `C-h k` describe key sequence
- `C-h a` apropos (find command matching a given pattern)
- `C-h i` info document reader; usually documents some emacs packages/plugins
- `C-h t` tutorial for Emacs
- `C-h r` read Emacs manual

### Autocompleting

- `C-M-i` autocomplete symbol (mainly for Emacs Lisp)

### Searching

- entering incremental search
    - `C-s` forward incremental search
    - `C-r` reverse incremental search
    - `C-M-s` forward incremental search regex
        - The regex syntax is somewhere in [A guided tour of Emacs](https://www.gnu.org/software/emacs/tour/)
- While in incremental search prompt:
    - `C-s` go to next occurrence
    - `C-r` go to previous occurrence
    - `M-p` previous item in search history
    - `M-n` next item in search history
- `C-h k C-s` describe your options while in incremental search prompt
- `M-%` query replace

### Marking, jumping, copying, cutting, and pasting

- rapidly moving between two locations
    - `C-SPC` set mark to current point
    - `C-x C-x` swap mark and current point
- while marking
    - cutting
        - `C-w` kill region (cut it into clipboard)
        - `M-w` copy region into clipboard
- `C-k` kill the rest of current line
- `C-y` paste from clipboard
- `C-S-BKSP` kill entire line

Some commands set the mark.

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

## Navigating the file system using dired

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

Read the dired info documentation.

Use `C-h m` or `M-x describe-mode` to get some help.

- Unanswered question:
    - What is the Emacs equivalent of VSCode Ctrl+P (open file fuzzy search / approximate string matching / subsequence matching)?
        - Completion styles.
            - [Completion Styles - GNU Emacs Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html)
        - arcane stuffs
            - [minibuffer - fuzzy completion-style - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/13500/fuzzy-completion-style)

## Programming in Emacs Lisp

- `M-\` delete all spaces and tabs around point
- `C-M-k` kill s-expression
- `C-x C-e` eval last s-expression before point
- indenting/formatting
    - `C-M-\` indent region
    - `M-x indent-sexp`
- partial reloading
    - `C-M-x` or `M-x eval-defun`
    - `M-x eval-region`
- setq
- add-to-list
- load
- load-path
- funcall

For someone used to Scheme, Emacs Lisp is unpleasant.
2018-09-12: When will Guile Emacs be mainstream?

- How do I define a local function?
    - You define it with let and lambda, as in Scheme.
    But you call it by prepending funcall.
    If in Scheme you write `(f a b)`,
    then in Emacs Lisp you write `(funcall f a b)`.
        - Why do I need this funcall noise?
        Because it's Lisp-2.
        If you don't like that, and you don't mind depending on the `cl` package,
        then you can use `flet` as described in [EmacsWiki: Local Functions](https://www.emacswiki.org/emacs/LocalFunctions).
        However, `cl` is a big package; it's a waste of dependencies if you only use `flet`.
- Warts for historical reasons
    - Lexical scoping is not the default.
    - Emacs Lisp doesn't have proper tail calls.
- Contentious
    - Lisp macro is simpler than Scheme macro, but Lisp macro is unhygienic.
- [Famous Programers on How Common Lisp Sucks](http://xahlee.info/comp/Common_Lisp_quotations.html)
- [EmacsWiki: Why Does Elisp Suck](https://www.emacswiki.org/emacs/WhyDoesElispSuck)
- [EmacsWiki: Emacs Lisp Limitations](https://www.emacswiki.org/emacs/EmacsLispLimitations)

### Suggested workflow

This example is for developing org mode:

Run several emacs processes:

- In workspace 1, run `emacs -mm` for writing Emacs Lisp code.
- In workspace 2, run `emacs -fh --no-desktop --visit ORGFILE` for testing. We'll often restart this instance.
- In workspace 2, run `emacs -Q -fh --eval '(info "/usr/share/info/org.gz")'` for reading the org mode manual.

## Going deeper

- Read the emacs manual in emacs.
- magit: [It's Magit! A Git Porcelain inside Emacs](https://magit.vc/)
    - vs git-el?
- erc: IRC with emacs
- opening PDF with emacs
- browsing the Internet with emacs
- icicles: "emacs library that enhances minibuffer/input completion"
- programming: paredit, haskell-mode, golang-mode, etc.
- Going even deeper
    - Write an Emacs Lisp program
    - Compare Emacs Lisp and Vim Script
- what
    - remembrance-agent: Emacs mode to help find relevant texts
    - twittering-mode: Twitter client for Emacs
    - [Famous Programers with Repetitive Strain Injury](http://ergoemacs.org/emacs/emacs_hand_pain_celebrity.html)
- [Vivek Haldar — The levels of Emacs proficiency](http://blog.vivekhaldar.com/post/3996068979/the-levels-of-emacs-proficiency)
- [Org mode for Emacs – Your Life in Plain Text](https://orgmode.org/)
- [Xah Lee's Emacs tutorial](http://ergoemacs.org/emacs/emacs.html)
- Learning emacs from others
    - [How does your Emacs setup look like? : emacs](https://www.reddit.com/r/emacs/comments/3hevcv/how_does_your_emacs_setup_look_like/)
    - [What does your Emacs setup look like? - Quora](https://www.quora.com/What-does-your-Emacs-setup-look-like)
    - [emacs-tw/awesome-emacs: A community driven list of useful Emacs packages, libraries and others.](https://github.com/emacs-tw/awesome-emacs)
- Other editors I haven't tried
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

## How to uninstall Emacs on Ubuntu 14.04

From [Uninstalling Emacs with apt-get: lessons in interface design](https://arp242.net/weblog/uninstalling_emacs_with_apt-get-_lessons_in_interface_design):

```
sudo apt-get remove 'emacs*'
```

## Trying spacemacs

Spacemacs is quite polished compared to vanilla Emacs.

Woe: Emacs can't install packages in parallel.

What is helm?
What is ivy?
What is projectile?
What is helm-projectile?

- [Helm vs Ivy: What are the differences, what are the advantages? : emacs](https://www.reddit.com/r/emacs/comments/7vcrwo/helm_vs_ivy_what_are_the_differences_what_are_the/)

## Using emacs for writing research papers

- [org-ref installation](https://github.com/jkitchin/org-ref#installation)
- [jkitchin/org-ref: org-mode modules for citations, cross-references, bibliographies in org-mode and useful bibtex tools to go with it.](https://github.com/jkitchin/org-ref)
- [Org mode for academic writing: Bibliographies with org-ref - Vivek's Info](http://viveks.info/org-mode-academic-writing-bibliographies-org-ref/)

## Increasing emacs usability

- [jwiegley/use-package: A use-package declaration for simplifying your .emacs](https://github.com/jwiegley/use-package)

Woe: Magit requires Git >= 1.9.4.
Ubuntu 14.04 comes with Git 1.9.1.
Build from source, or add a PPA.

Automatic indentation of comments
[surprises](https://stackoverflow.com/questions/26312317/wrong-indentation-of-comments-in-emacs)
people who don't know
[Emacs Lisp comment conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html).
