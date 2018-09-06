---
title: Speeding up Git
permalink: /gitspd.html
date: 2018-09-05 23:44 +0700
---

- TOC
{:toc}

## The problem

I have a repository with 100,000 files and 1,000,000 objects.
Git rebase is slow.
Git interactive rebase is even slower.

## Hypothesis: How git rebase works

I guess `git rebase --onto TARGET BASE MOVE` works like this:

```
git checkout --orphan TARGET --
git cherry-pick <all commits from BASE to MOVE, excluding BASE, including MOVE>
git checkout -B MOVE
```

Cherry-pick is also slow.
I guess that speeding up cherry-pick will also speed up rebase.

Checkout is also slow.
I guess that speeding up checkout will also speed up cherry-pick.

It seems that `commit` and `write-tree` are slow.

`GIT_TRACE_PERFORMANCE=true` has no effect.
Which git version is it for?

## The plan

- [atlassian.com: How to manage big Git repositories](https://www.atlassian.com/blog/git/handle-big-repositories-git)
    - Try git sparse checkout?
    It seems that sparse-checkout and rebase doesn't mix.
- Not recommended: `git gc --aggressive` (doesn't do what we think it would do).

## Plan: Make a rebase that uses only trees and not indexes

If a tree changes, all its ancestors have to be rewritten.
