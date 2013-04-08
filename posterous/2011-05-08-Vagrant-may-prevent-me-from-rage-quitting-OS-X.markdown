---
layout: post
title: Vagrant may prevent me from rage-quitting OS X

date: 2011-05-08
comments: true
external-url:
categories:
---


If you're a Mac user who came to OS X from any other Unix (one with a
really solid package manager, like apt on Debian GNU/Linux and
derivatives), I highly recommend checking out
[Vagrant](http://vagrantup.com/).

I've really chafed at the lack of decent package management on OS X, and
this will likely fill that gap for me. Homebrew is good, but it's just
not as easy as apt. I like using OS X and all the "syntactic sugar" that
comes with it, like Netflix Instant and fast Flash, but it's still hard
to do some things, like install LaTeX. If your package manager tells you
to go off and download a standalone installer, you know something's
wrong. (I don't blame Homebrew for this, I believe them when they say
building LaTeX from source is hairy, but I still don't like it.)

Vagrant is a tool for managing virtual machines geared toward web
development, but it's actually more useful than just that. It's super
easy to download a base Ubuntu box and in a few steps have an ssh
session into a new virtual machine instance, with shared folders already
set up and a command-line interface for suspending, resuming and tearing
down the virtual machine.

You have access to all the same packages in the Ubuntu repos without the
overhead of a Gnome session running inside VirtualBox. I know you can
set this stuff up manually with the VirtualBox GUIs or command-line
tools, but Vagrant makes it so easy!

You can install packages directly with apt-get through an ssh session or
provision the box with Puppet or Chef, neither of which I had ever used
before. Using simple manifest files or "cookbook" scripts would mean you
wouldn't need to worry about the state of the VM, and with careful
maintenance of the provisioning scripts, you could trash it and start
fresh without a lot of difficulty.

The only tip I have for using Vagrant in this way is that if you're
using Ubuntu, make sure to run "apt-get update" before doing any
provisioning, and start the provisioning system with all the verbose and
debug options you can enable, because otherwise you might end up with
some problem on the apt end but the "vagrant up" command would just hang
for several minutes with no output until everything fails and you find
out that apt couldn't find some package lists. At least, that's what
happened to me when I tried to install some LaTeX packages.
