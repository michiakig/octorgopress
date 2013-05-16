---
layout: post
title: "A tip for using Git on top of CVS"
date: 2011-06-04
comments: true
external-url:
categories:
---


I was recently asked to start a discussion on Git at work, and give a
presentation on it to my colleagues. We use CVS, and while there's a bit
of interest in migrating, that's relatively big task since we have a lot
of infrastructure set up around CVS.

In the meantime, I've started exploring the Git tools for CVS
interoperability. For a small, new project, `cvsimport` and
`cvsexportcommit` (as described in
[this](http://stackoverflow.com/questions/584522/how-to-export-revision-history-from-mercurial-or-git-to-cvs/586225#586225)
excellent SO answer) worked pretty well. However, when trying to do this
with an older project with a lot of history in CVS, `cvsimport` failed,
although I don't know why (The result seemed to be complete history for
only a subset of the files in the module). There are other tools for
migration, specifically `cvs2git`, but those require access to the CVS
repository directly.

Yesterday I found this
[question](http://stackoverflow.com/questions/6228264/how-to-run-git-and-cvs-over-the-same-folder)
about running Git on a directory that's also versioned with CVS, and
generating a patch to be committed back into CVS. This gave me an idea,
what if you skipped the `cvsimport` step? Could you check out the
project with `cvs co`, then run `git init` and hack away in bliss, and
then when you're ready to commit to CVS, use `cvsexportcommit`. That was
essentially what this person was asking about, although they didn't
mention `cvsexportcommit`.

It turns out this totally works, and in fact there are a few answers
elsewhere on SO that mention this method. It's not as nice as having all
the old, CVS history visible in things like `git log` and `gitk`, but if
`cvsimport` failed for you, and `cvs2git` isn't an option, it's probably
the next best thing.
