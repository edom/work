---
title: Emacs-Vi correspondence table
permalink: /emacsvi.html
date: 2018-09-08 01:38 +0700
---

- TOC
{:toc}

## Feature correspondence table

This table is incomplete.
A blank cell means I'm still looking for the content.
If you know vi, this may help you learn emacs quicker.

A common design principle is:
The more often a command is used, the shorter it should be.

|description|vi|emacs|
|-|-|-|
|quit the editor|:q|C-x C-c|
|**motion**|||
|small motion|h, j, k, l (small L)|C-b, C-n, C-p, C-f|
|screenful motion|C-f, C-b|C-v, M-v|
|go to beginning of line|0|C-a|
|go to end of line|$|C-e|
|go to beginning of buffer|gg||
|go to end of buffer|G||
|go to previous position in jumplist|C-o||
|go to next position in jumplist|C-i||
|go to matching bracket|%||
|**file system**|||
|visit file|:e \<file>|C-x C-f \<file>|
|vim netrw / emacs dired|:e \<dir>|C-x C-f \<dir>|
|write file / save buffer|:w|C-x C-s|
|find?|:f||
|**marking / selection**|||
|vim visual mode / emacs mark|v||
|vim visual line mode|S-v||
|vim visual block mode / column editing|C-v||
|**clipboard**|||
|copy line|Y||
|copy selection in visual mode|y||
|paste|p|C-y|
|**repetition**|||
|repeat prefix|\<number>|C-u \<number>|
|go left 5 chars|5h|C-u 5 C-b|
|repeat last change|.||
|repeat last command||C-x z|
|repeat complex command||C-x ESC ESC|
|**buffer**|||
|find buffer|:b|C-x C-RIGHT|
|next buffer|:bn|C-x C-RIGHT|
|previous buffer|:bp|C-x C-LEFT|
|delete buffer|:bd|C-x k|
|read into buffer|:r||
|read shell output into buffer|:r!\<command>||
|**insert**|||
|insert after cursor|a||
|insert before cursor|i||
|insert at begining of line|I (big i)||
|insert at end of line|A||
|insert line above|O (big o)||
|insert line below|o||
|**overwrite/replace**|||
|replace mode (overwrite)|R||
|replace from cursor to end of word|cw||
|replace from cursor to end of line|C / c$||
|**deleting/cutting**|||
|delete from cursor to end of word|dw||
|delete from cursor to end of line|D / d$|C-k|
|**find, search**|||
|find text|/\<text>||
|find next occurrence of word under cursor|*||
|find previous occurrence of word under cursor|#||
|find next occurrence|n||
|find previous occurrence|N||
|**undo/redo**|||
|undo|u||
|redo|C-r||

## Other similar tables

- [EmacsWiki: Vi Emacs Table](https://www.emacswiki.org/emacs/ViEmacsTable)
