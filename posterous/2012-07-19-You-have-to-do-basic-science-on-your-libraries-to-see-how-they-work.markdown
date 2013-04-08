---
layout: post
title: "You have to do basic science on your libraries to see how they work."

date: 2012-07-19
comments: true
external-url:
categories:
---


...so said Gerald Sussman, when [a Lisper
asked](http://wingolog.org/archives/2009/03/24/international-lisp-conference-day-two) why
MIT switched from Scheme to Python, and why SICP was replaced with a
robotics class. (the full quote or paraphrasing is really brilliant and
I think sums up a large part of the experience of my generation of
programmers, and not just the ones who went to MIT).

That's old news, and this post isn't commenting on that. It's just that
this has been ringing truer and truer for me recently, as I struggle to
get some useful work done using Android's WebView. I think the struggle
is paying off, since with the help of the source I've been able to [cut
to some truth in places.](http://stackoverflow.com/a/11572941)

I've also been struggling to get unit tests that exercise code
interacting with a WebView, and with JavaScript running in a WebView, to
work at all. When the code for a unit test is more complicated and
requires more careful thought, than the code under test, I think there
may be something sour going on.

When working with this framework (WebView on Android, and Android in
general) more often then not you end up with this wretched slurry of
(relatively) weakly, statically typed Java, untyped XML configuration
files, and weakly, dynamically typed JavaScript that's deeply removed
from the REPL. (Yep, I'm throwing brickbats with weasel words here) It
makes one yearn for the kind of strict and exacting type safety of
Haskell. It's back to Haskell for me, although not for Android apps,
unfortunately. Maybe Scala could fill that role, although I suspect it
would be just halfway on a log scale.

I really don't understand types well enough to whinge in this fashion,
so I've begun some levelling up there. [This
thread](http://news.ycombinator.com/item?id=4154794) on Hacker News gave
me a lot to ponder. I hope TAPL has the answers.
