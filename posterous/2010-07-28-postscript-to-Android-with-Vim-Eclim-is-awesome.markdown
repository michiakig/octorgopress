---
layout: post
title: "postscript to Android with Vim: Eclim is awesome!"
date: 2010-07-28
comments: true
external-url:
categories:
---


I have to admit that after working on some simple Android apps using Vim
and building on the command line, I realized to my dismay that I was
missing some of the bloat from Eclipse, specifically the automatic
handling of import statements.

I could just use Eclipse, but it's too slow for me. If text input
becomes even at all sluggish because the compiler is running in the
background, it's too slow. I'm not the only one who thinks this: Brad
Fitzpatrick said the same thing in [Coders at
Work](http://books.google.com/books?id=nneBa6-mWfgC&lpg=PA174&ots=gDzpEaRT-x&dq=eclipse%20%22coders%20at%20work%22&pg=PA85#v=onepage&q&f=false).
Some people might just say "get a more powerful development machine,"
but I don't think that's really fair. It might solve the problem, since
I've been working on a netbook (I'm about to upgrade to a Macbook Pro,
for all the same old [reasons](http://www.paulgraham.com/mac.html),
despite new mixed feelings about
[Apple](http://www.paulgraham.com/apple.html)). It still doesn't solve
the problem that I want to use Vim and not Eclipse. So I started looking
around for other people who hack Java in Vim. I quickly stumbled upon
[Eclim](http://eclim.org/), which is a brilliant tool.

Basically it runs Eclipse in a server-mode and provides Vim with code
completion, validation, import handling and
[more](http://eclim.org/features.html#java-integration) through a bunch
of Vimscripts. What's really nice is that Vim is still pretty fast, in
part probably because it doesn't actually do the continuous compiling
all the time, but instead only compiles and validates when you save a
file. I struggled a little bit to get it to find the Android libraries,
but eventually it worked. (I hadn't pointed the ADT plugin to the SDK
directory in Eclipse, but I think it would be fine if your Eclipse is
already properly configured first.)
