# Program transformation tool

## How do I set up vanilla vim?

By vanilla vim, we mean the vim you get by
`sudo apt-get install vim` from the official Ubuntu 14.04 repository
without customization (except copying the default vimrc
and changing a few default indentation options).

Make sure that you cd to the directory containing this readme first.

In the snippet below, `<Tab>` means you press the Tab key.

```
vim -S Session.vim
:set path=<Tab>,**
:set ai
:mksession!
```

## How do I browse the code with vanilla vim?

In the snippet below, `<file-pattern>`
is the name of the file you want to open.

```
:find <file-pattern>
:tabfind <file-pattern>
```
