---
layout: post
title: "Setting up Emacs for Lisp hacking on OS X, pt. 2: Common Lisp and Clojure"

date: 2010-11-30
comments: true
external-url:
categories:
---


This is likely out of date. You probably don't want to follow these
instructions. This is what I'm using now:

[http://spacemanaki.posterous.com/setting-up-emacs-for-lisp-hacking-on-os-x-pt...](http://spacemanaki.posterous.com/setting-up-emacs-for-lisp-hacking-on-os-x-pt-48220)

I updated this after using it for a while and finding out that there are
problems using the most recent CVS snapshot SLIME with swank-clojure,
and then after trying to answer someone's question about this problem
over on
[StackOverflow](http://stackoverflow.com/questions/4551283/what-is-wrong-with-my-emacs-slime-setup-compile-and-load-eval-not-working)

These are the rest of my notes for setting up Lisp hacking on OS X,
focusing on using SLIME with Common Lisp and Clojure. I'm using SBCL and
Emacs from Homebrew with `--cocoa`. I have no idea if this will work
with other Common Lisp implementations or with Aquamacs.

What seems to be the recommended way to use SLIME with Common Lisp is to
use the most recent version from CVS. However, `swank-clojure` only
works with the SLIME package in ELPA (according to this discussion on
the [swank-clojure Github
page](https://github.com/technomancy/swank-clojure/issues/issue/31)).
And this version from ELPA is stripped down and won't work with Common
Lisp.

In order to use SLIME with both Common Lisp and Clojure in a single
Emacs (and change from one to the other after restarting Emacs, but
without fiddling with settings in `.emacs`) I had to resort to a bit of
a hack.

These are the steps I did to get this to work:

With a fresh Emacs (no configuration at all, so move `~/.emacs` and
`~/.emacs.d` somewhere else for the moment) install ELPA:

[http://tromey.com/elpa/install.html](http://tromey.com/elpa/install.html)

From within Emacs, install the packages "slime" and "slime-repl".
(`M-x package-list-packages` then `C-s slime` then `i` to select and `x`
to install)

Move the files in `~/.emacs.d/elpa/slime-20100404` and
`~/.emacs.d/elpa/slime-repl-20100404` to a new directory like
`~/hacking/lisp/elpa-slime`.

Throw out the ELPA install: `$ rm -rf .emacs.d`.

Now clone the emacs-starter-kit and move it to `.emacs.d`. I only did
this with a fresh copy from technomancy's Github, so try that first if
you have problems.

(If you want to use Scheme too, and have followed the instructions in
part 1, just save your `username.el` somewhere and add the two functions
below once you get Common Lisp and Clojure working. You should be able
to have all three set up in the same Emacs, although not running at the
same time)

Get the latest SLIME with CVS:

    cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime

OS X doesn't come with CVS, but it's installed along with XCode and the
developer tools from Apple, so it might be installed on your system
already.

I moved `slime` to `~/hacking/lisp/cvs-slime`.

Hopefully it's obvious what the Emacs Lisp below does:

    (defun slime-common-lisp ()
              (interactive)
              (setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Common Lisp impl
              (add-to-list 'load-path "~/hacking/lisp/cvs-slime/")  ; your SLIME from CVS directory
              (require 'slime)
              (slime-setup '(slime-repl))
              (slime))
              
              (defun slime-clojure ()
              (interactive)
              (add-to-list 'load-path "~/hacking/lisp/elpa-slime")
              (require 'slime)
              (slime-setup '(slime-repl))
              (slime-connect "localhost" 4005))

Now `M-x slime-common-lisp` will start the Common Lisp runtime and give
you a SLIME REPL.

For Clojure you'd have to start the Clojure runtime and `swank-clojure`
on port 4005, Leiningen comes with a utility for this:

    $ ~/.lein/bin/swank-clojure

You can also create a new Leiningen project and start a `swank-clojure`
inside that, but if you just want a vanilla REPL to send code to from
Emacs, the above works.

Then in Emacs: `M-x slime-clojure`. You should now have a new buffer
with a Clojure REPL with the prompt `user>`. Remember that you have to
restart Emacs if you want to switch from one to the other. If someone
knows how to avoid this, please let me know (you'd have to unload SLIME
and reload the one that works with the language you want).
