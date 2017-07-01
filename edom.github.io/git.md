---
title: Building a mental model for using Git
permalink: /git.html
date: 2017-06-26 00:36:00 +0700
featured: true
---

In Git, there are two kinds of things:
*commits* and *references*.

*Commits* store your changes.

*References* name commits so that you can call a commit `master`
instead of `da39a3e`.

If you are a visual person,
you can think about how git commands change
the picture shown by `gitk`
(a tool for visualizing Git repositories).

In gitk, a blue circle is a commit,
a green box is a *branch*,
and the bold green box is the *current branch*.

A branch *is* a reference.
It is not a series of commits.

When you `git init`, Git creates a `.git` directory.

When you `git status`, it prints `On branch master`.
It means that the bold green box in gitk has the label `master`.

When you `git commit`, you make a new commit (blue circle),
and move the current branch (bold green box) to that new commit.

When you `git reset Target`,
you move the current branch (bold green box) to `Target`.

If you are not yet comfortable with Git,
back up your data by copying the whole folder.
Things will go wrong.
The `.git` directory can get corrupt.
You may accidentally type `git reset` and don't know how to recover.
Computers don't understand what you mean.
They do what you say, not what you mean.

I'm oversimplifying things and not telling the whole truth.
I just intend to help you build a mental model
that allows you to use Git with some understanding.
For the truth, read the [Pro Git](https://git-scm.com/book) book
or the [manpages](https://git-scm.com/docs) (`man git`),
or if you demand the absolute truth, the [source code](https://github.com/git/git).

> [All models are wrong; some models are useful.](https://en.wikipedia.org/wiki/All_models_are_wrong)
>
> George Box (1919--2013)
