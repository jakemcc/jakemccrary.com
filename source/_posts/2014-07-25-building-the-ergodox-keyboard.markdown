---
layout: post
title: "Building the ErgoDox Keyboard"
date: 2014-07-25 22:19
comments: true
categories: [ErgoDox, hardware]
published: false
---

I don't remember where I first heard about the ErgoDox but it was
probably from someone on Twitter. The [ErgoDox]() is a split hand keyboard
with the design released under the GNU GPL v3. As a result you can go
buy the parts yourself and build your very own.

There are a few standard (not sure if standard is the correct term
when it comes to building a custom keyboard) ways of getting the
parts. It basically comes down to sourcing all the parts yourself or
buying a bundle from [Massdrop](https://www.massdrop.com/buy/ergodox).
I opted to wait till Massdrop did a drop and bought a kit from them.

### Why? ###

I was drawn to building an ErgoDox for a few reasons. For the
last eight years I've used a Microsoft natural (ergonomic) keyboard
and I was interested in trying out a fully split design. The second
reason is that I wanted to have a keyboard with mechanical
key switches. I was also intrigued by the additional thumb keys.

### Buying ###

As mentioned earlier I bought my parts from Massdrop. With the buy I
participated in I had the option of a full hand case or the
traditional case and opted for the full hand. Another option I payed
more for was having aluminum top layers to the otherwise fully
acrylic top. Since the aluminum top was an extra piece I still had
the option of using all acrylic. I ended up using all acrylic on one
of my hands and actually like the look and feel of it better.

I also opted to buy a set of blank DSA keycaps through Massdrop. If I
were to do it again I would not buy the keycaps from Massdrop and
would instead buy them direct from
[Signature Plastics](http://www.keycapsdirect.com/). It would have
cost a bit more but then I would have branched out in color.

I did end up buying some "deep-dish" DSA keys direct from Signature
Plastics. These are keys that have a different top shape. This allows
you to tell when you fingers are above the home row. I'd recommend
doing this.

For key switches I bought Cherry MX Blues through Massdrop. Blues are
extremely clicky. You can easily hear me typing in every room of my
apartment. It is very satisfying.

### Building ###

I followed Massdrop's step-by-step
[guide](https://www.massdrop.com/ext/ergodox/assembly.php) and
[this](https://www.youtube.com/watch?v=x1irVrAl3Ts). Another great
resource is the community at
[GeekHack](http://geekhack.org/index.php?topic=22780.0). I'd recommend
reading and watching as much as possible before starting your build.

I built this using a cheap soldering iron I've had for years, very
thin solder, solder wick, and a multimeter. I don't know if this would have been
easier with better tools or not but those got the job done.

When it comes time to solder your surface mount diodes you only solder
them to the areas that have a key silk screen. I became carried away
and soldered a few to areas that were not going to get a key.

I eventually figured out a system for soldering the diodes that
minimized the pain of making an error.

1. Lay down some solder on one of the pads.
1. Put the correct end of the diode on top of that solder, reheat and
   push down.
1. Test the connection with a multimeter.
1. Solder the other half of the diode.
1. Test the connection.

I batched up the steps. I'd do a whole row of the first step, then
move to the second for the entire row, then do the third, etc. Being
rigorous about testing every connection is important. It will save
you a ton of trouble later. You want to verify as you go that you are
successfully completing steps. Waiting till the end would be a
disaster.

It was very useful to me to test the diode's connection after the
first half of it was soldered. It is fairly easy to detach a single
side of a diode. If you solder both sides and then you learn you put
it on backwards it is significantly harder to detach.

Eventually you'll need to cut open a USB cable. I ended up removing
the plastic housing using a Dremel. When soldering the wires to the
USB holes I was too concerned with it looking pretty and did not leave
plenty of wire. This made it harder to solder and as a result I ended
up doing a poor job that resulted in a short. After desoldering and
destroying another cable, but leaving more wire, I managed to do a
better job. I noticed the short because I kept getting warnings from
my computer about my USB Keyboard drawing too much power.

I've
[annotated a copy](https://www.evernote.com/shard/s68/nl/7741043/4f51c3b2-b50b-47d3-8219-ea155cf5fef5/)
of Massdrop's instructions using Evernote. It contains the above tips
inline.

### Firmware ###

After you physically build your keyboard you need to build the
firmware. There are a few different firmwares that can work and you
can discover those on GeekHack. I'm using a fork of what Massdrop's
graphical configuration tool uses. It is based off
[benblazak/ergodox-firmware](https://github.com/benblazak/ergodox-firmware).

You can use either Massdrop's configurator or build your own. By
building my own I was able to configure media keys that work with my
system. I also modified when LEDs are lit below keys to turn on when
it makes sense to me.

One of the exciting things about the ErgoDox is tweaking the firmware.
I encourage you to take a look at the source even if you use the
graphical configuration tool.

I haven't explored beyond
[benblazak/ergodox-firmware](https://github.com/benblazak/ergodox-firmware)
so I can't compare it to other firmwares.

### Conclusion ###

I really enjoy it. I don't use the thumb clusters as much as I
originally thought. I almost exclusively use the large (1x2) keys on
the thumbs. I also don't use the keyboard layers too often. I only
purposefully switch layers to hit my media keys.

Would I recommend building your own ErgoDox? If you already can or are
willing to learn to solder and this sounds at all interesting to you I
would recommend it. The project can be frustrating but the result is
great.


### Resources ###

- [GeekHack ErgoDox thread](http://geekhack.org/index.php?topic=22780.0)
- [GeekHack FAQ](http://geekhack.org/index.php?topic=40501.0)
  Useful for general information about keyboard topics.
- [GeekHack Cherry actuation forces](http://geekhack.org/index.php?topic=40501.0#post_DD)
- [Massdrop Buy](https://www.massdrop.com/buy/ergodox)
- [Massdrop Configuration Tool](https://www.massdrop.com/ext/ergodox)
- [benblazak/ergodox-firmware](https://github.com/benblazak/ergodox-firmware)
- [TMK firmware for ErgoDox](http://geekhack.org/index.php?topic=48106.0)
  One of the alternative firmwares.
