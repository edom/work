---
title: Emacs for vi users
permalink: /emacsvi.html
date: 2018-09-08 01:38 +0700
---

- TOC
{:toc}

## Which emacs package?

(Ignore this section.
Just use Spacemacs.)

- Woe: Ubuntu 14.04 emacs too old for spacemacs.
Must build emacs from source there.
- [Switching from Emacs to Vim (actually Spacemacs) - SaltyCrane Blog](https://www.saltycrane.com/blog/2015/12/switching-emacs-vim-actually-spacemacs/)
- [VSCode vs Spacemacs for TypeScript Development](https://spin.atomicobject.com/2018/07/31/spacemacs-typescript-dev/)

Spacemacs uses evil-mode?

Emacs 24 FAQ recommends VIPER for people who want vim's `.` command.
There are also other packages such as VI, VIP, evil-mode, and vile.
There is a "vile" (vi-like emacs) package on Ubuntu 14.04.

Deal breaker?
It seems that Ubuntu 14.04 VIPER doesn't have `*` and visual mode.

To enter viper mode, `M-x viper-mode`.

To exit viper mode, `M-x viper-go-away`.

To toggle viper mode, `M-x toggle-viper-mode`.

To change viper level, `M-x viper-set-expert-level`.

How do I install evil-mode (vi keybindings for emacs)?
Should I?
It's not on GNU ELPA.

- [package - Do I still need GNU ELPA if I have MELPA? - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/10500/do-i-still-need-gnu-elpa-if-i-have-melpa)
- [Is evil-mode too evil? : emacs](https://www.reddit.com/r/emacs/comments/6ej18a/is_evilmode_too_evil/)

## Feature correspondence table

This table is incomplete.
A blank cell means I'm still looking for the content.
If you know vi, this may help you learn emacs quicker.

A common design principle is:
The more often a command is used, the shorter it should be.

|description|vi|emacs|
|-|-|-|
|quit the editor|:q|C-x C-c|
|abort a command|ESC ESC|C-g / ESC ESC ESC|
|**motion**|||
|small motion|h, j, k, l (small L)|C-b, C-n, C-p, C-f|
|screenful motion|C-f, C-b|C-v, M-v|
|go to beginning of line|0|C-a|
|go to end of line|$|C-e|
|go to beginning of buffer|gg|M-\<|
|go to end of buffer|G|M->|
|go to line number|\<number>G|M-g g|
|go to previous position in jumplist|C-o||
|go to next position in jumplist|C-i||
|go to matching bracket|%||
|go to beginning of next word|w / W||
|go to beginning of previous word|b / B|M-b|
|go to ending of this word or next word|e / E|M-f|
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
|delete from cursor to end of word||M-d|
|delete from cursor to just before next word|dw||
|delete from cursor to end of line|D / d$|C-k|
|delete character under cursor|x|C-d|
|delete to character|df\<char>|M-z \<char> RET / M-x zap-to-char|
|delete to nth occurrence of char after cursor|d\<n>f\<char>|C-u \<n> M-z \<char> RET|
|**indenting**||
|indent|>|C-x TAB / M-x indent-rigidly|
|indent||C-u 4 C-x TAB|
|**find, search**|||
|incremental search forward|/\<text>|C-s|
|incremental search backward|/?<text>|C-r|
|find next occurrence|n|C-s|
|find previous occurrence|N|C-r|
|find next occurrence of word under cursor|*||
|find previous occurrence of word under cursor|#||
|**undo/redo**|||
|undo|u||
|redo|C-r||
|emacs undo/redo||C-/ \| C-_ \| C-x u|

## Other similar tables

- [EmacsWiki: Vi Emacs Table](https://www.emacswiki.org/emacs/ViEmacsTable)
