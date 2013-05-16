---
layout: post
title: "book reviews, Clojure 101: Programming Clojure, Practical Clojure, and Clojure for Lisp Programmers"
date: 2011-03-14
comments: true
external-url:
categories:
---


I've recently read these two introductory Clojure books and thought that
reviewing them might be helpful to anyone with a background similar to
mine, that is someone new to Clojure but not new to Lisp.

I wish I had found [Rich Hickey's talk "Clojure for Lisp
Programmers"](http://clojure.blip.tv/file/1313398/) before buying either
of these books. I think that both of them are decent books which present
a startlingly fresh and exciting language in a rather plain and
unexciting way. Neither of them is the kind of quirky Lisp book I
apparently really like.

For programmers with a decent understanding of another Lisp, I would
strongly recommend seeking other introductions to Clojure. If you know
any Lisp, skip the books and watch Rich Hickey's talk. I think it's a
much, much better introduction. Not only is it more concise but it
reaches deep into what sets Clojure apart from Common Lisp and Scheme. I
would imagine his talk targeted at Java programmers is probably also
great.

Hickey comes across as very smart and a bit opinionated, but what's
great is that he's also very convincing. There is a room full of Common
Lisp and Scheme people asking interesting questions in that video
(unfortunately some of those are unintelligible and the trascript
doesn't help). Watching that talk got me much more excited about Clojure
than reading these books did. And I actually understood some of the
design decisions in Clojure which had previously seemed a bit odd.

Michael Fogus's The Joy of Clojure might be the book I really wanted.
Although it's targeted at programmers new to Lisp, from glancing over
the table of contents, it looks like it covers much more than either of
these books, and so I may end up reading that too. In the meantime, I'm
going to start actually writing some Clojure:

    user=> (load-file "goblinfort.clj")
    #'user/make-name
    user=> (doseq [i (take 10 (repeatedly (fn [] (make-fight-sentence (make-name 2) (make-name 3)))))] (println i))
    Vuxqu lacerated Qafumes with a rough knife
    Aqnup crushed Coirvol
    Gelna cut off Daqikzub's leg
    Fuzov poked Gomluuj
    Zizbo poked Kakenok's finger with a sharp mace
    Pinub lacerated Jisojve's finger with the dull mace
    Eyic slashed Ofqeem
    Viwwe smashed Udwuluc's leg with the rough mace
    Movis chopped off Erreob's toe in a rough club
    Zear tore off Iwzevzaw's arm
    nil
    user=>

Goblins!
