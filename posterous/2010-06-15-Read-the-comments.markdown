---
layout: post
title: Read the comments

date: 2010-06-15
comments: true
external-url:
categories:
---


It's kind of weird that the online edition of Real World Haskell has a
comments section for every paragraph. There are some interesting things
discussed in the threads attached certain exercises and examples, and
occasionally the authors have chimed in, but for the most part, it's
just a lot of chatter and distraction. I thought it was a little strange
in the PHP manual, and that only has a comments section for every page.
(Albeit there is probably more noise in the PHP manual comments than
RWH... zing!)

Actually, there is a lot of whining about discussing advanced topics too
soon in the RWH comments, which I think is silly. After all, it's not
[http://learnyouahaskell.com/](http://learnyouahaskell.com/). Anyway for
the most part I find them distracting because if I don't immediately
understand a sentence or paragraph, I might open the comments and end up
feeling more confused, instead of just re-reading the section, backing
up and re-reading what preceded it, or moving on and finding answers
later in the page. It turns reading into a much more fragmentary
experience. I guess what I really should do is just get the print
edition.

If the comments distracted you as much as they did me, here's a tip:

wget --mirror -p --convert-links -P
./rwh http://book.realworldhaskell.org/read/

Otherwise though it's really great, and I've only read the first three
chapters. There are some really cool exercises, even though it's early
in the book, such as implementing the Graham scan algorithm for finding
a convex hull from a set of points.

[http://en.wikipedia.org/wiki/Graham\_scan](http://en.wikipedia.org/wiki/Graham_scan)

This seems like a kind of "not-so-real-world" problem,
although Wikipedia does list a few practical uses I didn't know about.
It's still a neat problem and it's interesting to work on it in
Haskell. I'll probably start pushing my solutions (at least to the
more interesting problems) to my
[Github](http://github.com/spacemanaki).
