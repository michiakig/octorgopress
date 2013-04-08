---
layout: post
title: How to build BlackBerry applications with Eclipse on Mac OS X

date: 2010-12-07
comments: true
external-url:
categories:
---


The other day a [pretty scathing
critique](http://spin.atomicobject.com/2010/11/22/the-cost-of-building-blackberry-apps)
of the current state of BlackBerry app development was submitted to
[HN](http://news.ycombinator.com/item?id=1959433). While the post (from
developers at a company called Atomic Object) was absolutely spot on,
more interesting to me was a link to one of their [older
posts](https://spin.atomicobject.com/2010/11/04/our-blackberry-development-environment)
describing their development setup on OS X. I had messed around with
this a little bit before, trying to get Eclipse and Ant to properly
compile BlackBerry apps on OS X, but without success. I didn't try hard
enough apparently.

The post from the Atomic Object team is pretty detailed in explaining
how to do this, using IntelliJ and Parallels. The fundamentals are not
specific to those tools though, so I followed along and have adapted it
to Eclipse and VirtualBox. This gets filed under inane notes about
development environments, but as I said when I posted about Emacs and
Lisp on OS X, this kind of thing has benefited me in the past, maybe it
will help someone else.

1.  Install Eclipse (I am using 3.5, but it may not matter) and
    VirtualBox.
2.  Create a new VM and install Windows (tested with XP SP3). This would
    probably work using VMWare Fusion or Parallels too.
3.  On the VM, install Java 6 and the version of the BlackBerry JDE that
    matches your target OS.
4.  In OS X, download
    [bb-ant-tools.jar](http://bb-ant-tools.sourceforge.net/) and move it
    to \~/.ant/lib.
5.  Get an OS X version of preverify, which is included in the [Sun J2ME
    SDK 3.0 for OS
    X](http://www.oracle.com/technetwork/java/javame/downloads/sdk30-jsp-139759.html).
    Install it and either copy
    /Applications/Java\_ME\_SDK\_3.0.app/Contents/Resources/bin/preverify
    to somewhere in your PATH or just add that directory to your PATH.
6.  If you don't have one already, create the file
    \~/.MacOSX/environment.plist.
7.  Edit this file with /Developer/Applications/Utilities/Property\\
    List\\ Editor.app/.
8.  Create a new variable called PATH and set it to the value of your
    shell PATH, making sure that the directory containing preverify is
    included. This allows Ant, via Eclipse, to see the preverify command
    when Eclipse is launched from Eclipse.app and not from the command
    line. See
    [this](http://developer.apple.com/library/mac/#documentation/MacOSX/Conceptual/BPRuntimeConfig/Articles/EnvironmentVars.html#//apple_ref/doc/uid/20002093-113982)
    for more details.
9.  In OS X, create a directory for the BlackBerry components (something
    like "bb-components").
10. From the BlackBerry JDE installation in the VM, copy both "lib" and
    "bin" directories to this directory.
11. In Eclipse, create a new Java project.
12. Choose "Use an execution environment JRE:" and select Java 1.3.
13. Right click the project in the "Package Explorer" and select "Build
    Path" and then "Configure Build Path."
14. Add bb-components/lib/net\_rim\_api.jar as an "External JAR."
15. Remove the "JRE System Library." This is so that only BlackBerry
    supported classes will be offered via autocompletion etc.
16. Copy the attached minimal
    [build.xml](https://gist.github.com/727661) into the project.
17. Edit the build.xml to suit your environment (specifically the
    jde.home property) and anything else you want to customise.
18. Right click and select "Run as" and then "Ant Build" (the first
    one). You can also build using Ant on the command line, of course.

Now you should be able to build BlackBerry apps in Eclipse without too
much fuss. Testing on the simulator would require you to copy the files
to the VM, which can be done by creating a shared drive and copying them
there, or connecting to the VM using shh and scp. I use the former,
because it's a little easier to set up. The simulator is not perfect
though, and while it runs in VirtualBox, you should really be testing it
on a device anyway. So you can use JavaLoader.exe via VirtualBox to
deploy to the device.

I had to jump through some hoops to get a BlackBerry connected via USB
to work in VirtualBox. What worked for me was to install the [BlackBerry
Desktop Software for
Mac](http://us.blackberry.com/apps-software/desktop/desktop_mac.jsp) and
make sure that it was able to sync the devices I use for testing. Then I
was able to enable it in VirtualBox under "Devices" and "USB Devices."
