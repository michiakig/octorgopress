---
layout: post
title: Graham's scan, Graham Hutton, Wikipedia, and CLRS

date: 2010-07-01
comments: true
external-url:
categories:
---


The author's of RWH point to the Wikipedia articles on [convex
hulls](http://en.wikipedia.org/wiki/Convex_hull) and [Graham's
scan](http://en.wikipedia.org/wiki/Graham_scan) as a reference for the
exercise I mentioned last week. Wikipedia's great, but [Introduction to
Algorithms
(CLRS)](http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=11866)
has a much clearer presentation of the algorithm I think (Wikipedia has
pseudo-code from Algorithms by Sedgewick and Wayne but I haven't
actually read that so I won't presume to judge it based on someone's
"adaptation"). It's tempting to edit the article with the version from
CLRS, only to have it reverted a few days later, but I think I will
refrain.

Finding a convex hull from a set of points is a nice problem because
convex hulls can be described so easily in two dimensions by visualizing
a rubber band around nails driven into a board. The Graham scan is also
quite easily understood, at least at the abstract level. I found this
great animation which the user [David
Ashley](http://en.wikipedia.org/wiki/User:David_Ashley) uploaded and
posted to the Discussion page which really illuminates how
the algorithm works: 

[[posterous-content:ujpkcaACjAHCjfeJogHo]]

I'm not going to belabor this too much, and I certainly won't do a code
review of my Haskell solution, that would be too precocious, if you're
really interested it's on Github.

Completing this exercise coincided with the delivery from Amazon of
Graham Hutton's Programming in Haskell, which is very nice, exactly what
\
it claims to be: a simple, clear, and concise introduction to the
language. I'm already several chapters into it, my only real criticism
is the strange choice to use certain symbols instead of actual Haskell
syntax (such as an arrow instead of minus-greater-than) but that's
easily overlooked, or maybe not, depending on your taste.
