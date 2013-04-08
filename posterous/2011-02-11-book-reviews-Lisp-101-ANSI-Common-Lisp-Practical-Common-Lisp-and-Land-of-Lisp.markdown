---
layout: post
title: "book reviews, Lisp 101: ANSI Common Lisp, Practical Common Lisp, and Land of Lisp"
date: 2011-02-11
comments: true
external-url:
categories:
---


I'm going to start posting reviews of books that I read in 2010,
starting with three introductions to Common Lisp. These three books are
all quite good, but a little bit different from each other, so I thought
reviewing them together might be helpful if someone is on the fence
about buying one or all of them.

Paul Graham's ANSI Common Lisp
------------------------------

I was interested to see how Paul Graham, surely one of Lisp's most
ardent champions, would present the language for beginners. Like his
essays, the prose ANSI Common Lisp is clear and very well written. The
programming examples are short and concise, and quite easy to follow.
The examples are quite engaging and interesting: a sentence/poem
generator named "Henley", a simple ray-tracer, an object-oriented
framework, and a logic language stick out in my memory.

But, I don't think it's necessarily the best introduction to Lisp in
2011, and not given the price of a new copy on Amazon. I found a cheap
used copy, and that's why I bought it. I wasn't really disappointed,
because I'm happy to make room for it on my shelf, but if you are new to
Lisp, my recommendation would be to check out Practical Common Lisp or
Land of Lisp, and after reading one of those, peruse the source for ANSI
Common Lisp from Graham's site.

His much more advanced macrology book, On Lisp, is more well known I
think, and has fewer peers than this introductory text. That is even
more expensive new (or used) due to the limited printing, I think, but
Graham has graciously provided the text free from his site. I haven't
read it yet, but it's high on my list of Lisp books to read.

Peter Seibel's Practical Common Lisp
------------------------------------

If you're a serious, professional programmer who wants to learn Lisp,
and doesn't want to muck around with games (see Land of Lisp below) or
with relatively academic examples, then this is probably the book you
want. Peter Seibel's book has a nice conversational tone and he doesn't
waste much time evangelizing. I think a blurb on the back of his other
book, Coders at Work (review coming soon) says something like "Seibel
asks the sort of questions only a fellow programmer would" and I think
that's kind of true of this book too, it's definitely written as one
programmer to another. He compares certain features to Python, Java, C++
and a few other languages, but doesn't dwell on disparaging them too
much, instead relying on Common Lisp features to stand on their own.

The examples in Practical Common Lisp are definitely practical, and
could even be called a bit dry. This isn't bad at all, and might even be
more to some people's taste. If concrete, "business-like" examples
appeal to you more, this book would be a better read than Land of Lisp.
If you get excited by more academic examples and prefer a lighter tone
to examples in programming books, Land of Lisp might be a better bet. Or
just read them both.

An impressive feat is that Seibel presents macros almost as soon as
possible (in the third chapter) and demonstrates why Lisp's macros are
unique. I think Paul Graham has said that he tried to race to the macro
chapter in ANSI Common Lisp as quickly as possible, but here Seibel has
beaten him to it with a clever example that I thought was pretty easy to
follow, even though it was my first exposure to "true" macros.
Additionally, this chapter almost stands on its own, so you can forward
a link to the online version (the whole book is up on Seibel's site for
free) to your coworkers who want to know what the fuss is about Lisp
macros, but aren't willing or interested in diving deep themselves.

Dr. Conrad Barski's Land of Lisp
--------------------------------

I'd read Dr. Barski's online mini-tutorial "Casting SPELs in Lisp" a
little while ago, so when I saw that he had finished Land of Lisp, and
saw what an absolutely wonderful, whimsical music video he had produced
for it, I bought a PDF copy immediately.

Using games to explore Common Lisp (or any programming language) is a
pretty good idea, because games engage a wide variety of programming
problems. I think reasonably motivated high schoolers could probably get
through most of Land of Lisp, and I sort of wish it had been written
when I was in high school. That being said, this book does has plenty
for "grownups". One of the cooler examples is using lazy evaluation to
improve the efficiency of searching a game tree for a computer
opponent's best move. Barski also presents an SVG-based web interface to
this game, and a simple HTTP server written using a socket library,
getting into low-level details of web programming, which isn't something
you usually encounter in an introductory text to any programming
language.

If you read and enjoyed "Casting SPELs in Lisp" or watched the music
video on the Land of Lisp site, and you had a big smile on your face,
you'll probably like this book. If you hate fun, stick with Practical
Common Lisp.

I think any of these books are fine first Lisp books, but there's
nothing stopping you from reading all of them. (I did after all) This
year I plan to dive into some of the big, epic Common Lisp books, so I
expect I'll have a second round of Lisp reviews in a year, maybe a "Lisp
201" to follow this.
