---
layout: post
title: "Setting up Emacs for Lisp hacking on OS X, pt. 3: Common Lisp and Clojure, again"
date: 2011-07-20
comments: true
external-url:
categories:
---


I wrote about getting Emacs and SLIME to play nice with both Clojure and
Common Lisp a while ago, but that post had a kind of hacky setup that
likely doesn't work any more.

The good news is that if you want to hack Clojure and Common Lisp with
SLIME, on the same Emacs installation, this is now a lot easier to set
up. First, clone [this](https://github.com/technomancy/slime) frozen
copy of SLIME and load it in your Emacs init file with this snippet.
Don't use the ELPA/package.el/Marmalade SLIME as that won't work with
Common Lisp.

For Clojure, install Leiningen (or Cake), and the swank plugin. Now you
can run a swank server with Leiningen (or Cake) projects with
`lein swank` (or `cake swank`) and connect to it with
`M-x slime-connect`. I had issues with threads getting stuck or
deadlocked or something, but this wasn't an issue after I installed the
clojure-swank version 1.3.1. You can test this by running the ant sim
demo and checking that the ants do indeed go scurrying around.

For Common Lisp, make sure your `inferior-lisp-program` is set to your
implementation, and just use `M-x slime`. You can even run them at the
same time without any problems, as far as I can tell.

References:

Phil H's great work and recent [blogpost](http://technomancy.us/149) on
`clojure-jack-in` (Note that if you follow those instructions I think
this will should still work, although you might not be able to run them
side by side.)

Sam Aaron's Overtone screencast is where I found out about this frozen
copy of SLIME.


