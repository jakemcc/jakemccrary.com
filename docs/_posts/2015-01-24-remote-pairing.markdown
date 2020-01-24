---
layout: post
title: Remote Pairing
date: 2015-01-24 15:19
comments: true
published: true
categories:
- pairing
- remote
---

Over a year ago I joined [Outpace](http://outpace.com). All of
Outpace's developers are remote but we still practice pair
programming. As a result I've done a lot of remote pairing. I was
skeptical before joining that it would work well and I'm happy to
report that I was wrong. Remote pairing works.

## Why remote pairing?

The usual pair programming
[benefits](http://c2.com/cgi/wiki?PairProgrammingBenefits) apply to
remote pairing; more people know the code, quality is higher, and
it provides an opportunity for mentorship. Another benefit, more
beneficial in a remote setting, is that it increases **social
interaction**.

The most common response I receive when I tell someone I work from my
apartment is "I'd miss the interaction with co-workers." When you work
remote you do miss out on the usual in office interaction. Pair
programming helps replace some of this. It helps you build and
maintain relationships with your remote colleagues.

## Communication

Communication is an important part of pair programming. When you're
pairing in person you use both physical and vocal communication. When
remote pairing you primarily use vocal communication. You can pick up
on some physical cues with video chat but it is hard. You will never
notice your pair reaching for their keyboard.

I've used Google Hangouts, [Zoom](http://zoom.us), and Skype for
communication. Currently I'm primarily using Zoom. It offers high
quality video and audio and usually doesn't consume too many
resources.

I recommend not using your computers built-in microphone. You should
use headphones with a mic or a directional microphone. You'll sound
better and you'll stop your pair from hearing themselves through your
computer.

I use
[these headphones](http://www.amazon.com/gp/product/B005VAORH6/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B005VAORH6&linkCode=as2&tag=jakemccrary08-20&linkId=3AX26BCB4ZHZWLC5).
They are cheap, light, and open-eared but are wired. I've been told I
sound the best when I'm using them. I also own these
[wireless headphones](http://www.amazon.com/gp/product/B003VANOFY/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B003VANOFY&linkCode=as2&tag=jakemccrary08-20&linkId=LDHRWNCUOO45B7G4).
They are closed-ear, heavier, and wireless. The wireless is great but
the closed-ear design causes me to talk differently and by the end of
the day my throat is hoarse. Both of these headphones are widely used
by my colleagues and I don't think you can go wrong with either one.

Some people don't like wearing headphones all day. If you are one of
those I'd recommend picking up a directional microphone. Many of my
colleagues use a
[Snowball](http://www.amazon.com/gp/product/B002OO333Q/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B002OO333Q&linkCode=as2&tag=jakemccrary08-20&linkId=XVNYHVNXTAC3J2RD).

## Connecting the machines

So now you can communicate with your pair. It is time to deal with the
main problem in remote pairing. How do you actually work on the same
code with someone across the world?

At Outpace we've somewhat cheated and have standardized our
development hardware. Everyone has a computer running OS X and, if
they want it, at least one 27 inch monitor (mostly Apple 27 inch
displays or a
[Dell](http://www.amazon.com/gp/product/B009H0XQQY/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B009H0XQQY&linkCode=as2&tag=jakemccrary08-20&linkId=JJLKLYNESYPKCRKZ))
with a resolution of 2560x1440. Since everyone has nearly identical
hardware and software we are able to pair using OS X's built-in screen
sharing. This allows full sharing of the host's desktop. This full
desktop sharing is the best way to emulate working physically next to
your pair. This enable the use of any editor and lets you both look at
the same browser windows (useful for testing UIs or reading reference
material). With decent internet connections both programmers can write
code with minimal lag. This is my preferred way of pairing.

Another option that works well is [tmate](http://tmate.io/). tmate is
a fork of tmux that makes remote pairing easy. It makes it dead simple
to have remote developer connect to your machine and share your
terminal. This means you are stuck using tools that work in a terminal
and, if you are working on a user interface, you need to share that
some other way. There generally is less lag when the remote developer
is typing.

A third option is to have the host programmer share their screen using
screen sharing built-in to Google Hangouts or [Zoom](http://zoom.us).
This is a quick way to share a screen and is my preferred way of
sharing GUIs with more than one other person. With both Zoom and
Google Hangouts the remote developer can control the host's machine
but it isn't a great experience. If you are pairing this way the
remote developer rarely touches the keyboard.

## Soloing

It might seem weird to have a section on soloing in an article about
remote pairing. Soloing happens and even in an environment that almost
entirely pairs it is important. Not everyone can or wants to pair 100%
of the time. Soloing can be recharging. It is important to be
self-aware and recognize if you need solo time. Below are a few tips
for getting that solo time.

One way to introduce solo time is to take your lunch at a different
time than your pair. This provides both of you and your pair with an
opportunity to do a bit of soloing.

Other short soloing opportunities happen because of meetings and
interviews. It isn't uncommon for half of a pair to leave for a bit to
join a meeting, give an interview, or jump over to help out another
developer for a bit.

Soloing also happens as a result of uneven team numbers. If your team
is odd numbered than there are plenty of opportunities for being a solo
developer. Try to volunteer to be the solo developer but be aware of
becoming too isolated.

## Conclusion

Remote pairing works. Working at [Outpace](http://outpace.com) has
shown me how well it can work. Reasonably fast Internet paired with
modern tools can make it seem like you're almost in the same room as
your pair.
