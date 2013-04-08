---
layout: post
title: "Setting up Emacs for Lisp hacking on OS X, pt. 1: Scheme"
date: 2010-11-27
comments: true
external-url:
categories:
---


This weekend I took a bus from NYC to Boston and then on to Portland, ME
for the holiday (it's Thanksgiving weekend here in the US). I find it
hard to do any real work on a bus because I tend to get a little car
sick (I was quite a puke-y kid growing up) so I decided to start
fiddling with my Emacs set up, which wouldn't take too much deep
thought.

I'm going to be posting my notes over the next few days; they might be
useful for someone else. I've benefited greatly from people posting
howtos like this on random blogs, plus when I hose it I can always refer
back to this.

Install some implementation of Scheme if you don't have one installed
already. I've been using MIT Scheme for SICP, but you can get Gambit
Scheme, Scheme 48 and PLT Scheme (via mzscheme) through Homebrew. MIT
Scheme has a pre-built binary for OS X which includes their Edwin Emacs
clone and a command line interpreter.

Vanilla Emacs is also in Homebrew, and probably in Macports and Fink as
well. For Homebrew, you'll probably want to specify `--cocoa` when
installing it to get Emacs.app, which I'd advise you move into the OS X
`/Applications` directory, which will allow you to start Emacs via
Spotlight.

I would highly recommend checking out Phil Hagelberg's "Emacs Starter
Kit" which includes a huge number of customizations and niceties for
getting started with Emacs. The best way to use this is to fork the
[project](https://github.com/technomancy/emacs-starter-kit "emacs-starter-kit")
on Github, create your own branch (or not), clone it on your machine and
then move the directory to `~/.emacs.d`, being careful if you already
had some Emacs stuff in there you want to keep.

He provides a way of adding your own customizations by creating a
`~/.emacs.d/username.el` where `username` is replaced by your system
username. Now you can add all your own stuff in there, or add stuff to
override things you don't want from the Emacs Starter Kit (personally I
don't like the faded parens, visual bell, and pretty lambdas, but that's
all largely personal preference). Now you can keep your changes separate
and pull updates the the starter kit in the future, without messing
everything up.

To get a Scheme REPL inside of Emacs, you need to configure Emacs to use
the particular Scheme binary of our implementation. Since I'm using MIT
Scheme, this means
`/Applications/mit-scheme.app/Contents/Resources/mit-scheme`. If you are
using something else, use the path to that binary instead. I tried it
with mzscheme from Homebrew and it worked, but I don't know about the
others.

Add this to your `~/.emacs.d/username.el`, replacing string with the
path to your Scheme binary:

    (setq scheme-program-name 
          "/Applications/mit-scheme.app/Contents/Resources/mit-scheme")

MIT Scheme has an extended interface for interacting with the REPL
called "xscheme", which includes things like `M-o` for evaluating an
entire buffer. This should be included with Emacs. (If it's not, you can
get it
[here](http://ftp.gnu.org/gnu/mit-scheme/utilities.pkg/xscheme.el) )

Add this to your `~/.emacs.d/username.el` file to use this interface:

    (defun load-xscheme () (require 'xscheme)) 
    (add-hook 'scheme-mode-hook 'load-xscheme)

Now `M-x run-scheme` will open your chosen Scheme implementation in a
new buffer and let you send the expression before the cursor with
`C-x C-e`.

If the extra xscheme commands don't work, or if Emacs throws up when you
start it, maybe there is some issue with the xscheme.el library. Try
downloading it separately, saving it into the `~/.emacs.d` directory and
adding this to your `~/.emacs.d/username.el` file:

    (load "~/.emacs.d/xscheme.el")

Restart Emacs and try the xscheme commands again.

I'll add part two, for setting up Common Lisp and Clojure using SLIME,
sometime soon.
