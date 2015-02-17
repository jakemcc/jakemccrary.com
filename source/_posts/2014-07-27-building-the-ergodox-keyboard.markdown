---
layout: post
title: Building the ErgoDox Keyboard
date: 2014-07-27 21:05
comments: true
categories:
- ErgoDox
- hardware
published: true
---

Earlier this year I built an
[ErgoDox](http://ergodox.org/Default.aspx). The ErgoDox is a split
hand mechanical keyboard whose design has been released under the GNU
GPLv3. There are a few standard [^1] ways of getting the parts. It
basically comes down to sourcing all the parts yourself or buying a
bundle from [Massdrop](https://www.massdrop.com/buy/ergodox). I opted
to wait until Massdrop was selling them and bought a kit from them.

![My ErgoDox](/images/my-keyboard.jpg "My ErgoDox")

### Why? ###

1. I've used an ergonomic keyboard for years and was intrigued by the split hand design.
1. I wanted to try out Cherry MX key switches.
1. Using your thumb for more than just space bar made a lot of sense to me.
1. The firmware lets you have multiple layers. I thought this could be really useful.
1. The project sounded fun. I used to make physical devices and this
   seemed like a good way to do that again.

### Buying ###

As mentioned earlier I bought my parts from Massdrop. In the buy I
participated in I had the option of a full hand case or the
traditional case and I opted for the full hand. As part of the buy I
also bought additional aluminum top layers, a blank set of DSA [^2]
keycaps, and Cherry MX blue key switches.

If I were doing it again I would not buy the extra aluminum top
layer. I built one of my hands using the aluminum and the other with
the basic acrylic top. I enjoy both the look and feel of the acrylic
hand better.

I would also not buy the set of DSA keycaps from Massdrop. It was
convenient and a bit cheaper to buy from them but had I known I could
get different
[colors](http://keyshop.pimpmykeyboard.com/products/full-keysets/dsa-blank-sets-1)
from [Signature Plastics](http://www.keycapsdirect.com/) I would have done that.

I also bought eight "deep-dish" DSA keys direct from Signature
Plastics. These keys feel different which lets me know when my fingers
are above the home row. I'd recommend doing this. You can order from
[this](http://www.keycapsdirect.com/key-capsinventory.php) page.

For key switches I bought Cherry MX Blues through Massdrop. Blues are
extremely clicky. You can easily hear me typing in every room of my
apartment. It is very satisfying.

After using the keyboard for about a week I also ended up buying some
[pads](http://www.amazon.com/gp/product/B00897D3OQ/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B00897D3OQ&linkCode=as2&tag=jakemccrary08-20&linkId=HTBBKS475FGFIG2M)
for my wrists. I occasionally rest my wrists on the keyboard and the
keyboard's edge would dig into me.

### Building ###

I followed Massdrop's step-by-step
[guide](https://www.massdrop.com/ext/ergodox/assembly.php) and
[this](https://www.youtube.com/watch?v=x1irVrAl3Ts) YouTube video.
Another great resource is the community at
[GeekHack](http://geekhack.org/index.php?topic=22780.0). I'd recommend
reading and watching as much as possible before starting your build.

I built this using a cheap soldering iron I've had for years, very
thin solder, solder wick, and a multimeter. I don't know if this would
have been easier with better tools or not but those got the job done.

While soldering the surface mount diodes I was in the zone and
soldered a few locations that didn't actually need to be soldered.
When you are soldering the diodes you should only be soldering them to
the locations that have the key silk screen.

My system for minimizing errors while soldering the diodes is the
following five steps.

1. Lay down some solder on one of the pads.
1. Put the correct end of the diode on top of that solder, reheat and
   push down.
1. Test the connection with a multimeter.
1. Solder the other half of the diode.
1. Test the connection.

I batched up the steps. I'd do a whole row of the first step, then
move to the second for the entire row, then do the third, etc. Being
rigorous about testing every connection is important. Catching
mistakes early makes it easier to fix the mistakes.

If you solder a diode on the wrong way there is a huge difference (at
least for me using solder wick) between the difficulty of fixing the
error when only one pad has been soldered versus two pads. I soldered
more than one diode backwards and a mistake noticed after soldering
only one pad was easy to fix. After soldering both pads it took
serious effort.

Eventually you'll need to cut open a USB cable. I ended up removing
the plastic housing using a Dremel. When soldering the wires to the
USB holes I was too concerned with it looking pretty and did not leave
plenty of wire. This made it harder to solder and as a result I ended
up doing a poor job that resulted in a short. After desoldering and
destroying another cable, but leaving more wire, I managed to do a
better job. I originally noticed the short because I kept getting
warnings from my computer about my USB Keyboard drawing too much
power.

I've
[annotated a copy](https://www.evernote.com/shard/s68/sh/4f51c3b2-b50b-47d3-8219-ea155cf5fef5/df239167726bcebf06cc2b5101ac8e42/)
of Massdrop's instructions using Evernote. It contains the above tips
inline.

### Firmware ###

After you physically build your keyboard you need to build the
firmware. There are a few different firmwares that can work and you
can discover those on GeekHack. I'm using a fork of what Massdrop's
[graphical configuration](https://www.massdrop.com/ext/ergodox) tool
uses. It is based off
[benblazak/ergodox-firmware](https://github.com/benblazak/ergodox-firmware).

One of the exciting things about the ErgoDox is tweaking the firmware.
I took the base firmware and modified it to have media key support and
[light up the LEDs](https://github.com/jakemcc/ergodox-firmware/commit/383f16a3f091b4e2dd031d098007c4289cc1a261)
when I'm on any layer besides the base. Some people have added the
ability to record keyboard macros and other neat features. I encourage
you to take a look at the source even if you use the graphical
configuration tool. I haven't explored beyond
[benblazak/ergodox-firmware](https://github.com/benblazak/ergodox-firmware)
so I can't compare it to other firmwares.

### Conclusion ###

I really enjoy it. Building it was both fun and frustrating [^3].

After using the keyboard for a few months I've found that I really
only use three (on each hand) of the thumb cluster keys. I also don't
use the keyboard layers too often. I have three layers programmed and
I always stay on the main one unless I want to hit a media key.

Would I recommend building your own ErgoDox? If you already can or are
willing to learn to solder and this sounds at all interesting to you I
would recommend it. The project can be frustrating but the result is
great.

### The Future ###

There is still a lot left to explore in the custom keyboard space.
Even so I have no plans on leaving the ErgoDox anytime soon. In terms
of improving my ErgoDox, I plan on poking around the different
firmwares at some point. I'd also like to explore
[tenting](http://geekhack.org/index.php?topic=22780.msg1405792#msg1405792)
[options](https://github.com/adereth/ergodox-tent).


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

[^1]: I feel a bit odd using the word standard to describe acquiring parts to build a keyboard.
[^2]: [This](http://keycapsdirect.com/key-caps.php) page has diagrams that shows the different keycap families.
[^3]: Those surface mount diodes are so tiny.
