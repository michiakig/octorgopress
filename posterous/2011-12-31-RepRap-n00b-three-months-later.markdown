---
layout: post
title: RepRap n00b, three months later

date: 2011-12-31
comments: true
external-url:
categories:
---


My RepRap is working! Over the Thanksgiving weekend I had a solid chunk
of time to address the problems I'd been having. After a full day of
tweaking I was able to print a nice looking single walled box and a very
ugly looking minimug.

The minimug was ugly because I hadn't set the feed diameter in
Skeinforge and it had defaulted to 1.75 mm and I'm using 3 mm PLA. So
the printer was extruding a lot more plastic than it thought it was.
After I fixed that I printed another minimug and it came out much nicer,
although it was neither water nor alcohol tight.

I've since printed a bunch of stuff and have experimented a bit with
some of the 3D design software. I'm still a RepRap n00b but at least I
have a working printer! It's pretty exciting. I'm very impressed with
the quality of the prints I've done, and this isn't bragging because the
quality of the prints has very little to do with my efforts and a lot
more to do with the all the work that's been put into the RepRap
project.

The rest of this post is a collection of notes for other people who are
really new. These are places where I really stumbled, so this list might
be valuable to someone else.

1.  After I assembled my printer I was tempted to just jump in and try
    printing something but was immediately disappointed. I didn't
    realize how much careful hardware and software calibration was
    required. Eventually, this guide is what got me from a totally
    non-functioning printer to a working machine:

[http://buggerit.blogspot.com/2011/08/hains-prusa-mendel-reprap-calibration.html](http://buggerit.blogspot.com/2011/08/hains-prusa-mendel-reprap-calibration.html)

1.  I disovered that leveling the print bed is really crucial. This is
    stressed a few times on the wiki and in other documentation,
    including the guide above, but it's easy to underestimate. I think
    this is especially true if you're printing on cold blue tape
    (without a heated bed). I gather that a heated bed is more reliable,
    but I think a lot of novices will opt for printing PLA on blue tape
    as I did, simply because it cuts down a little bit on cost and
    complexity. (That being said I think I'm going to get a heated bed
    very soon and would recommend anyone else just skip cold blue tape
    because it poses some ugly problems like prints getting totally
    stuck.)

    In my case, the level of the bed can be the difference between
    perfect print and total garbage. When I started, total garbage was
    the extruder's nozzle dragging a growing tangle of plastic around
    the bed, with nothing sticking to the bed at all. After I carefully
    leveled the bed so that the nozzle at Z=0 was only one sheet of
    paper above the bed, I got pretty decent prints right away.

2.  While I was calibrating the machine, my Z-axis started making a
    horrible metallic grinding noise. I was lucky to have some bicycle
    grease handy and smeared a dab on each of the Z-axis threaded rods
    which silenced them. Perhaps this is obvious, but I would recommend
    getting some machine oil or grease if you don't have any, just in
    case.

3.  As I was assembling my printer, I made a few mistakes that weren't
    deadly but damaged some printed parts slightly. When I was putting
    the extruder together, the small gear needed to be reamed out to fit
    on the spindle. Since it was such a tight fit, I didn't think I
    needed to use the set screw to hold it in place, especially since I
    would need to file out the nut trap to fit a nut in. Over time, the
    gear loosened and I ended up having to use a set screw. This was
    fine, until I needed to disassemble the extruder to clean out the
    hobbed bolt. After I did, I realized that the nut trap was no longer
    able to hold the nut in place, and I couldn't screw the set screw
    into place. This meant the motor would just spin its shaft inside
    the gear, and the extruder wouldn't work at all. Eventually, I was
    able to force the set screw into place to hold the gear on, but if I
    hadn't I would have been in trouble.

    The lesson I learned was this: if you realize that a printable part
    might fail, print it as soon as possible, unless you have access to
    another machine that works. I'm sure that if you're hacking on your
    RepRap all the time, this is obvious, but it's easy to get carried
    away and put this off if you're a novice. Right now I'm printing a
    few other parts that were weakened when I was building the printer
    (for example: one of those "h" shaped endstop holders), since the
    printer is working now I don't want to take any chances.

4.  I initially bought a Makergear hot end kit because it seemed to be
    recommended by a lot of people and was available in the US. I had
    some issues when I first started printing which could be attributed
    to the hot end, but it's certainly possible (and even very likely)
    that it was an error on my part. I eventually gave up and bought a
    J-Head hot end from RepRap-USA.com, and that has worked really well
    for me. The J-Head hot end is also much, much simpler to use, as it
    involves almost no any assembly. Frankly, the Makergear hot end is
    quite fiddly to assemble.

5.  This follows from the last note. Since I had two hot ends, I also
    had two thermistors, that happened to be identical. When I received
    the J-Head in the mail, I decided to use the thermistor that came
    with the Makergear kit and put the new one aside. A few nights and a
    few assemble/disassemble cycles later, one of the thermistor's leads
    broke.

    Since I had a spare, all I had to do was swap it out, but if I
    hadn't I would have had to order a new one. Thermistors are super
    cheap, so it wouldn't have been a big deal, but it would have been a
    bit of a pain in the ass. In the future, I'll be sure to have
    several spares of parts like these, those that are cheap but
    essential. If you're going order something from McMaster or another
    online supplier, and the part you're ordering is around a dollar or
    two, save yourself the hassle and buy two or three even if you only
    need one.


