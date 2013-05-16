---
layout: post
title: The Road to Lisp

date: 2010-09-06
comments: true
external-url:
categories:
---


I haven't posted in over a month mostly because I've been spending a lot
of my evenings studying Lisp. Althought the first post here was on
Haskell, it actually would have been more appropriate had it been on
Lisp, because Lisp (Scheme) was a largely responsible for me
rediscovering the joy in programming. So I've been thinking about
writing a Lisp post for a while, something like a ["Road to
Lisp"](http://wiki.alu.org/The%20Road%20to%20Lisp%20Survey) response.
But instead of a lengthy post I'll just get to the point and actually
respond to the survey. Thus...

I, (spacemanaki), do solemnly offer my responses to "The Road to Lisp
Survey":

**When did you first try Lisp seriously, and which Lisp family member
was it?**

In the winter of 2009 I read [The Little
Schemer](http://www.ccs.neu.edu/home/matthias/BTLS/) and wrote all of
the exercise programs out on paper in Scheme. Later I actually installed
MIT Scheme and played around in the REPL.

**Many of us had multiple run-ins with Lisp before it "stuck". The
"stick" date is of most interest, but you can share earlier encounters
if you like.**

I had an early encounter with Scheme in college. But we spent less than
a month with it (needless to say they weren't teaching a variation on
6.001 at the Big State Java School) and I didn't appreciate it as much
as I did after The Little Schemer.  At the time I was curious, but
somehow got distracted. All I took away was cons, car, cdr, and
tail-call optimization. I didn't loathe its syntax though, as many
programmers do. Maybe that alone was a sign of things to come.

**What led you to try Lisp?**

I watched some iteration of Douglas Crockford's talk based on his book
[JavaScript: the Good
Parts.](http://www.amazon.com/exec/obidos/ASIN/0596517742/wrrrldwideweb)
He put up a slide with code similar to this:

[code]var digit\_name = function () {     var names = ['zero', 'one',
'two',         'three', 'four', 'five', 'six',         'seven', 'eight',
'nine'];     return function(n) {         return names[n];     }; }();
alert(digit\_name(3));[/code]

And he said that if there was only one thing the audience should take
away from the talk, it was this. I paused the video and had to take a
long look at the code, because I had never seen anything like it. The
first sentence in the Wikipedia [article on
closures](http://en.wikipedia.org/wiki/Closure_(computer_science)) is a
gem: "In computer science, a closure is a first-class function with free
variables that are bound in the lexical environment." This was mostly
incomprehensible to me.

On Crockford's site there is a
[review](http://www.crockford.com/javascript/little.html) of the Little
Schemer, and I picked it up with a desire to better understand what he
meant that JavaScript was more like Scheme than like Java or that it was
"Lisp in C's clothing." After reading it and its sequel, [The Seasoned
Schemer](http://www.ccs.neu.edu/home/matthias/BTSS/), I not only
understood closures, but was completely delighted by Scheme.

**What other languages have you been using most?**

I'm paid to write Java and C at my day job, and occasionally do some
simple web projects using JavaScript (with HTML/CSS). I've been staying
up late learning Haskell and Ruby.

**How far have you gotten in your study of Lisp?**\
**I know this is hard to quantify. Just wing it.**

I've read the aforementioned Little Schemer and its sequel. I've since
moved on to the [Structure and Interpretation of Computer
Programs](http://mitpress.mit.edu/sicp/full-text/book/book.html); I've
only read two (small) fifths of that, but I'm far from giving up yet,
it's just slow going. Most recently I've started to learn Common Lisp
from Paul Graham's [ANSI Common
Lisp](http://www.paulgraham.com/acl.html) and [Peter Seibel's Practical
Common Lisp](http://www.gigamonkeys.com/book/). I would say I have only
just begun my study.

**What do you think of Lisp so far?**

I think Lisp is very compelling. I think it's delightful and incredibly
fun. I don't feel frustrated by the language, instead I feel empowered
and challenged.
