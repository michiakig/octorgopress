---
layout: post
title: Android development with ... Vim?

date: 2010-07-23
comments: true
external-url:
categories:
---


It was enormously refreshing to find this while I was setting up
the Android SDK and developer tools recently:

[http://developer.android.com/guide/developing/other-ide.html](http://developer.android.com/guide/developing/other-ide.html)

For the past several months I've been working on our
BlackBerry application at work. Despite my very limited experience with
Android (I'm really only at the "Hello, World." level) I think that
based on the link above, I'd be safe to assume that the support
and documentation from Google is much better than what RIM offers.

RIM has their own custom IDE for BlackBerry development (written in
Swing so it's pretty clunky). There's also an official Eclipse plugin,
but the most recent release doesn't support older versions of the
BlackBerry OS, making testing on a range of devices tricky (or
impossible). I've mostly been using Netbeans and its generic J2ME/mobile
plugin, because that's what my co-worker suggested. It works, but just
barely: you can install older component packages to test your app on
older phones, which is worth putting up with the flaky debugger
connection (and you should be testing on a phone anyway).

You actually can use any editor for BlackBerry development, and build
your app using ant,
[bb-ant-tools](http://bb-ant-tools.sourceforge.net/), and
[antenna](http://antenna.sourceforge.net/). In fact this is how we do
automated builds for testing and releases, but you could easily develop
this way too. I admit that I'm picking on RIM and BlackBerry a little
bit, but this really is just the tip of my grievances with the platform.
I don't think I am
[alone.](http://online.wsj.com/article/SB10001424052748704629804575324990808738422.html)

Anyway, finding semi-official support for other editors and other OSes
(BlackBerry development is completely Windows-based) was quite a
pleasant surprise. While I'm sure that the Android Eclipse plugin is
great, my muscle memory is heavily invested in Vim's command set and
modal editing style. And my preferred development environment is the
flavor du jour of GNU/Linux (currently Ubuntu seems to be the strongest
contender, but more on that later perhaps).

I'll be spending some more time with the Android SDK in the coming
weeks, so I'll be able to follow up about my initial impressions and
confirm if I was correct.
