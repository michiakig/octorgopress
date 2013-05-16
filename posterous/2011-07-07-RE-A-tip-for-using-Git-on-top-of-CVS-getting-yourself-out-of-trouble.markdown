---
layout: post
title: "RE: A tip for using Git on top of CVS, getting yourself out of trouble"
date: 2011-07-07
comments: true
external-url:
categories:
---


Occasionally, `git cvsexportcommit` will fail. I don't know how it works
inside, but I have noticed that it will sometimes have a problem
committing to CVS in the case of added or deleted files.

In this post, I will outline a method of manually exporting a patch file
from a local Git repository that has been based off of a project in CVS.
This requires a bit of fiddling, but once you package everything in a
small script, it's quite painless. Of course, the least pain would be to
just switch to Git, but this might help someone else stuck hassling with
a legacy project.

Since you need a CVS working directory to use `cvsexportcommit`, I
assume you already have one. To start, checkout the project twice, once
with a different name, like this:

    $ cvs co -d project-cvs project
    $ cvs co -d project-git project

This results in two copies of `project`. You can then delete the CVS
directories and create a new Git repository in one of them:

    $ cd project-git
    $ cvs release # not necessary
    $ rm -rf $(find . -name 'CVS')
    $ git init

Now you can work away in the Git working directory as normal, with all
the ease of Git, local branching, etc... I would recommend branching from
`master` and working there, then merging back into `master` before
exporting to CVS, just to keep things clean. You can then import into
`master` as well, and rebase your local branches against it to
incorporate any changes from your colleagues. Importing from CVS would
be the same as exporting as outlined below, just reverse the `diff`-ing
and `patch`-ing.

When you're ready to export your work and commit to CVS, you need to
create a patch by diffing your Git working dir with the CVS working
directory, and commit this patch to the CVS repository.

After a little hunting, I found this pair of incantations:

    $ diff --exclude='CVS' --exclude='.git*' -urPp cvs git

... where `cvs` and `git` refer to your working directories. You invoke
this in the directory above them, and it will spit out the patch to
stdout, so you probably want to redirect it to a file by appending
`> patch` the end. It's also helpful to use the `-q` option to `diff`
which will just list the changed files, and give you a quick sanity
check that you're setting up to commit what you think you are.

To apply the patch to your CVS working directory, change to it, and run

    $ patch -p1 < ../patch

... where `patch` is that patch file, presumably created in the directory
above your working directory. Then you can commit to CVS normally, with
`cvs ci -m '...'` etc.

This all might seem like a lot of work just to use Git, but in the
transition between CVS and Git, or in a situation where you want to use
Git in "guerilla" mode, this works in a pinch.
