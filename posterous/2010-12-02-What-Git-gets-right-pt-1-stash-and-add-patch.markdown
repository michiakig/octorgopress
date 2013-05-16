---
layout: post
title: What Git gets right, pt. 1, "stash" and "add --patch"

date: 2010-12-02
comments: true
external-url:
categories:
---


Earlier this year I started using Git for all my personal projects, only
after giving Mercurial a try first. Joel Spolsky's [hginit
tutorial](http://hginit.com) and the beginning of Bryan O'Sullivan's
book [Mercurial: the Definitive Guide](http://hgbook.red-bean.com/read/)
brought me to the light side (distributed version control). In the end I
picked Git over Mercurial because of Github (it's very sweet UI won me
over), deployment to Heroku, and because (as far I can tell) Git is a
little bit more popular. I'm sure Mercurial is great too, and also puts
CVS and SVN to shame, but I really don't have time in my life for two
cutting edge version control systems, at least not right now.

Git (and distributed version control in general) is great for personal
projects for a few reasons. [Some
people](http://reprog.wordpress.com/2010/05/10/git-is-a-harrier-jump-jet-and-not-in-a-good-way/)
think it's overpowered, but I think they're wrong (actually, in defense
of Mike Taylor, I think he also saw the
[light](http://reprog.wordpress.com/2010/05/13/you-could-have-invented-git-and-maybe-you-already-have/)).
I think distributed systems are way better for small, one-person
projects since you don't need any of the overhead of setting up a
server, even if it's just a server program running on a local machine.
To start using Git, all you need to do is install it and then run
`git init` in your project's directory. I've set up CVS locally and it's
a horrible huge pain in the ass in comparison. You don't want the tool
to get in the way and encourage bad practice (like not using version
control at all!).

Setting it up is a one-time cost of course, so if you have CVS or SVN
already set up, you might not be that compelled to switch to Git.
However, there are loads of other things about Git which make it
awesome. I'm going to start making a note of these things here as I come
across them, because writing about it will help me learn Git and also
help remember these things and their use cases.

Tonight I was hacking on some of the exercises from the [metacircular
evaluator](http://www.youtube.com/watch?v=0m6hoOelZH8&t=35m0s) in SICP,
and had a pernicious bug due to some changes I had recently made to
implement internal definitions. I had my implementation of this mostly
completed, but weird things were breaking, unrelated to internal
`define`s. I wanted to go back to an older version, but I didn't want to
commit the changes I had made, since they were obviously broken, but I
also didn't want to throw them out.

I remembered reading about `git stash` so I looked it up, and indeed
that's what I wanted, the description in the manual pretty much sums it
up: "Stash the changes in a dirty working directory away." Very sweet.
You can put a side changes in your working directory and come back to
them later.

So now I had a the previous version, I could see if it still worked. It
did, but after playing around with it a little bit, I found another bug.
The metacircular evaluator code from the SICP site is all in one file,
which is annoying and should probably be fixed to make hacking around on
it easier, but I would like to move on to the lazy evaluator, logic
programming, and register machine some day this year, so I don't spend a
whole lot of time making the code really extensible, I'm just playing
around and solving as many of the exercises that I can.

My point is, after I fixed the bug in this one, giant file, there were a
ton of other changes in it: a bunch of crap I had just messed with and
even some debugging printfs. I didn't want to commit that stuff. I
needed to somehow tease the few good lines out of a big file with many
more "bad" lines changed.

This time, I wasn't too sure, but I also had a little deja vu. I had
bookmarked Ryan Tomayko's [post from
2008](http://tomayko.com/writings/the-thing-about-git) a little while
ago and remembered something about reordering commits after the fact.
This wasn't exactly what I wanted to do, but it was close, and sure
enough, his post describes a totally killer feature of Git (apparently
present in other modern systems like Mercurial, Darcs and Bazaar),
`git add --patch`. This gives you an interactive dialog which allows you
to pick and chose which hunks get added to the index from a given file.
It even lets you break a hunk up if need be. It's totally awesome.

So there are the first two little bits of Git-fu that I've stumbled
across. There is a certain joy in finding a tool which is powerful and
excellently designed, and I think Git falls in this category. I'm really
looking forward to learning more and I'll be posting notes on what else
Git gets right when I do.
