---
title: Building a mental model for using Git
permalink: /git.html
date: 2017-06-26 00:36:00 +0700
---

A *commit* is a snapshot of the working tree.

A *reference* names a commit so that you can write `master`
instead of `da39a3e`.

If you are a visual person,
you can think about how git commands change
the picture shown by `gitk`
(a tool for visualizing Git repositories).

In gitk, a blue circle is a commit,
a green box is a *reference*,
and the bold green box is the *head*.

The head points to the commit that will be the parent of the next commit.

When you `git init`, Git creates a `.git` directory.

When you `git status`, it prints `On branch master`.
It means that the head points to the same commit pointed by the reference named `master`.

When you `git commit`, you make a new commit (blue circle),
and move the current branch (bold green box) to that new commit.

When you `git reset Target`,
you move the head (bold green box) to `Target`.

If you are not yet comfortable with Git,
back up your data by copying the `.git` directory.
It can get corrupted.
Things will go wrong.
You may accidentally do something and don't know how to recover.
Computers don't understand what you mean.
They do what you say, not what you mean.

## More information

A Git *object*:

- is identified by a SHA-1 hash;
- is either a blob, tree, or commit;
- is stored as a file somewhere in the `.git` folder.

A *commit* has zero or more parents.
It also refers to a tree.

A *tree* is a list of references.
Every reference points to either a tree or a blob.

For more information, read the [Pro Git](https://git-scm.com/book) book
or the [manpages](https://git-scm.com/docs) (`man git`).
