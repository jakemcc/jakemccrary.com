---
layout: post
title: "Successful Remote Pairing"
date: 2015-01-18 16:56:18 -0600
comments: true
published: false
categories: 
---


Title Ideas:

* "Reflections on a year of remote pairing"
* "Remote pairing recomendations"
* "Tips from a year of remote pairing"



A little more than a year ago I joined [Outpace](http://outpace.com).
100% of Outpace's developers are remote but we still practice nearly
100% pair programming. The rest of this post provides recommendations
that can help you successfully practice remote pairing.

## Why remote pairing?

The pair programming benefits from the
[c2 wiki](http://c2.com/cgi/wiki?PairProgrammingBenefits) apply to
both remote and co-located pairing. I've noticed a couple more
benefits from remote pairing.

It helps increase **social interaction**. Every time I tell someone I
work out of my apartment they respond with some statement about how
they couldn't do that because they would miss the interaction with
co-workers. Remote pairing helps you build and maintain relationships
with members of your organization.

I find that pairing helps me work **saner hours**. If I'm soloing on
something it is very easy for me to give up my evenings because I'm so
focused on solving a problem. It is too easy to get carried away. With
a pair of developers working together there is a good chance one of
them will want or need to stop working on time and that can help the
other half of the pair stop as well.

## Communication

Communication is an important part of pair programming. When you're
pairing in person you are able to use physical and audible
communication with your pair. When remote pairing you rely on audio
communication. With video chat you can pick up on some physical cues
but it is much harder to notice your pair reaching for their keyboard.

I've found that it almost doesn't matter what video chat software you
use. I've used Google Hangouts, [Zoom](http://zoom.us), and Skype. Of
the three Zoom is what I currently prefer. It tends to perform well
and is easy to invite others to join your video chat (just pass
someone a url).

Since most of your communication is through your voice it is key that
your pair can hear you well without hearing themself. Because of this
I recommend **not** using your computers built in microphone. If you
use the built-in microphone you are greatly increasing the likely hood
that your pair will experience a less than ideal listening experience.
Using an external microphone helps prevent feedback and often sounds
clearer than built-in mics.

I use
[these headphones](http://www.amazon.com/gp/product/B005VAORH6/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B005VAORH6&linkCode=as2&tag=jakemccrary08-20&linkId=3AX26BCB4ZHZWLC5)[^1].
They are cheap, light, and open-eared but are wired. I've been told I
sound the best when I'm using them. I also own these
[wireless headphones](http://www.amazon.com/gp/product/B003VANOFY/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B003VANOFY&linkCode=as2&tag=jakemccrary08-20&linkId=LDHRWNCUOO45B7G4).
They are closed-ear, heavier, and wireless. The wireless is great but
the closed-ear design causes me to talk differently than normal and by the end of the
day my throat is hoarse. Many of my co-workers use both of these
headphones and I don't think you can go wrong with either one.

If you are a person that doesn't like wearing headphones then you
should pick up a directional microphone. Many of my co-workers use a
[Snowball](http://www.amazon.com/gp/product/B002OO333Q/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B002OO333Q&linkCode=as2&tag=jakemccrary08-20&linkId=XVNYHVNXTAC3J2RD).

[^1]: This link and all others to Amazon in this post is an affiliate link.

## Connecting the machines

Now we know how to communicate with your pair. The next important need
for successful remote pairing is the actual pairing part. How do you
and your pair get on the same machine and look at the same code?

At Outpace we've standardized our development hardware. Everyone has
a computer running OS X and (if they want it) at least one 27 inch
monitor (mostly Apple 27 inch displays or a
[Dell](http://www.amazon.com/gp/product/B009H0XQQY/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B009H0XQQY&linkCode=as2&tag=jakemccrary08-20&linkId=JJLKLYNESYPKCRKZ))
with a resolution of 2560x1440. Since everyone nearly identical
hardware we are able to pair using OS X's built-in screen sharing.
This allows full sharing of the host's desktop. This allows you to use
any editor to edit code and see any user interfaces (example: a web
app) that deal with the code your touching. With decent Internet both
the host and remote programmer can type and interact with the host's
system with minimal lag. This is my preferred way of pairing.

Another option that works well is using [tmate](http://tmate.io/).
tmate is a fork of tmux that makes remote pairing easy. It makes it
dead simple to have remote developer ssh to your machine and share
your terminal. This means you are stuck using tools that work well in
a terminal and you need to find another way to share graphical user
interfaces. There generally is less lag when the remote developer is
typing.

Another option to have the host programmer share their screen using
screen sharing built-in to Google Hangouts or [Zoom](http://zoom.us).
This is a quick way to share a screen with another. The remote
developer can actually control the host's machine through these tools
but it involves a couple more steps and I find it a bit clunky. I find
when you pair this way the remote developer rarely touches the
keyboard.

## Soloing

When you are working in a near 100% pair programming environment
(remote or local) it is important to know if you are a developer that
needs some soloing time. This isn't uncommon. Some of the following
tips for finding soloing time work with local pairing as well.

One way to introduce solo time is to take your lunch at a different
time than your pair. This provides both of you and your pair with an
opportunity to do a bit of soloing.

Other short soloing opportunities happen because of meetings and
interviews. It isn't uncommon for half of a pair to leave for a bit to
join a meeting, give an interview, or jump over to help out another
developer for a bit.

Soloing also happens as a result of uneven team numbers. A team of
five will always have someone solo unless a member is out for the day.
If you're a developer who needs the occasional solo day then you
should try to volunteer frequently to be the solo developer. Be careful
though, doing this too often can lead to feelings of isolation as the
amount of social interaction takes a drastic hit.

## Conclusion

Remote pairing works. Before joining [Outpace](http://outpace.com) I
wasn't sure how well it would work. It turns out it works really well.
Modern technology almost makes it feel as if your pair is in the same
room as you.
