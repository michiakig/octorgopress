---
layout: post
title: RepRap n00b, one month later

date: 2011-10-20
comments: true
external-url:
categories:
---


I've read numerous posts on Boing Boing and elsewhere about cheap 3D
printers from both the RepRap project and companies like Makerbot but I
never paid a whole lot of attention to them. I usually shy away from
electronics projects, and instead have stuffed myself solidly in the
"software" pigeonhole. The only hardware projects I've ever done are
building gaming PCs in high school (nothing fancy like overclocking or
watercooling) and working as a bicycle mechanic at a tool co-op (I can
disassemble and reassemble a 70s era road bike or a modern track bike,
but that's the limit of my knowledge). Both of those are pretty far from
physical computing or electrical engineering though. Last month I ended
up digging a little deeper into DIY 3D printing and ended up developing
a strong urge to build a RepRap.

In the past I've avoided electronics projects mostly due to fear of
destroying circuit boards. I kind of regret that attitude, which is not
only a bit cowardly but misguided too. After playing with an Arduino and
a breadboard I realized that if you're careful, there's really plenty of
opportunity for the kind of experimentation and tinkering I'm used to in
the world of software. And the feedback loop is just as compelling as it
is in programming: examine the system, think about the interactions,
tinker, watch it light up/spin around/make noise! It's so much fun, and
gives me the same kind of feeling I got when I started programming. Why
didn't I try this stuff earlier!?

Anyway, in the past month I've built a RepRap Prusa Mendel. But I'm not
printing yet. I've got a ways to go. It is basically up and running:
after some minor problems with the wiring and electronics this week I
was able to test the motors and the heater. My first attempt at a real
test print (a small, flat square I made in Google Sketchup) resulted in
the extruder just dragging cooled PLA around the bed. I found a few blog
posts and a forum thread addressing this issue. Basically the Z axis end
stop needs to be lower, so that the extruder is much closer to the
surface of the bed. Also, I think the hot end is too hot, as filament
extrudes while it is just idling. So, I've got work to do.
