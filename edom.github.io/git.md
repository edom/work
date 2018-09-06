---
title: Using Git
permalink: /git.html
date: 2017-06-26 00:36:00 +0700
---

- TOC
{:toc}

## Mental model

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

## Things to write?

- Git fundamentals:
    - Git store things in the `.git` directory.
    - Why merge conflicts? How to resolve them? How to use `meld`? How to do a three-way merge?
    - Avoid changing spaces. Avoid using your IDE to reformat files that are already commited.
- Workaround for bad user experience
    - Disable git-gui GC warning:
        - https://stackoverflow.com/questions/1106529/how-to-skip-loose-object-popup-when-running-git-gui
- [Understanding Git Filter-branch and the Git Storage Model](https://manishearth.github.io/blog/2017/03/05/understanding-git-filter-branch/)

## Git hash collisions

Git hash collision may occur albeit extremely unlikely.
Git assumes that if two objects have the same hash, then they are the same object.
This is false; the converse is true: if two objects are the same, then they have the same hash.
When hash collision occur, Git may silently lose data.
Git is an example of software that is incorrect but works for the use cases it was designed for (source code versioning).
Git is not meant to be used as an arbitrary database.

Other softwares are incorrect as well.
We routinely make software that assumes that there will never be more than 2^64 rows in a database table.

Is it even possible to write correct software at all?

## Related tools

- git-gui, for making commits
- gitk, for showing history
- meld, for three-way diff/merge
