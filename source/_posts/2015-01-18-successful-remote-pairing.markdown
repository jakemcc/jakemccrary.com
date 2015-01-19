---
layout: post
title: "Successful Remote Pairing"
date: 2015-01-18 16:56:18 -0600
comments: true
published: false
categories: 
---

Until working at [Outpace](http://outpace.com) all of my pairing
experience happened co-located with my pair. We had two keyboards, two
mice, two monitors, and a single computer at one desk. Every developer
at Outpace works remotely and we still pair most of the time. There
are still two keyboards, two mice, and multiple monitors but now there
are multiple computers and multiple desks. The rest of this post is
going to give some recommendations on how to successfully pair remotely.


## Why remote pairing?

The pair programming benefits from the
[c2 wiki](http://c2.com/cgi/wiki?PairProgrammingBenefits) apply to
both remote and co-located pairing. I've noticed a couple more
benefits from remote pairing.

It helps increase **social interaction**. Every time I tell someone I
work out of my apartment they respond with statement about how they
couldn't do that because they would miss the interaction with
coworkers. Pairing helps you build and maintain relationships with
members of your team.

** WORK ON THIS **

I find that pairing helps me work **saner hours**. If I'm soloing on
something it is very easy for me to give up my evenings because I'm
trying to solve a problem. It is easy to get carried away. Pairing
helps with that as between the two of us there is a decent chance one
of us will have a commitment and need to leave. This helps the other
member



## Connecting Machines

We've standardized at Outpace so that every person has a computer
running OS X and everyone has (if they want it) at least one large 27
inch monitor with a resolution of 2560x1440 [CONFIRM]. Since everyone
nearly identical hardware we are able to pair using OS X's built-in
screen sharing. This allows full sharing of the host's desktop. This
means you can both see the code you're working on and (if applicable)
interact with the user interface (web app) that exercises what you're
developing. This is great when you're working on user interfaces or
working on backend services that support a web app. With decent
Internet both the host and remote programmer can type and interact
with the host's system. There can be a bit of lag for the remote
developer but it usually isn't much of a problem.

Another option that has worked well is using
[tmate](http://tmate.io/). tmate is a fork of tmux with a focus on
making pairing remotely easier. It makes is dead simple to have remote
developer sharing a console with you. Downsides to this are that
you're stuck pairing with just tools that work in a console. It does
generally provide a quicker response to the remote developer typing.

Another option to have the host programmer share their screen using
screen sharing built-in to Google Hangouts or [Zoom](http://zoom.us).
This is a quick way to share a screen with another. The remote
developer can actually control the host's machine through these tools
but it involves addition steps (clicking some options) and I find it
kind of clunky.

## Communication

As with co-located pair programming communication with your remote
pair is key. With co-located pairing you can see your pair reach
forward for a keyboard and pick up on other body language. You don't
have this luxury with remote pairing. This means you need to rely (??)
on vocal communication. We pair and have a constant video chat going
using Zoom. It is a decent substitute to being at the same desk as
someone. Having a directional mic or headphones prevents your pair
from hearing themselves talk through your computer speakers. Because
of my living situation I
[these headphones](http://www.amazon.com/gp/product/B005VAORH6/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B005VAORH6&linkCode=as2&tag=jakemccrary08-20&linkId=3AX26BCB4ZHZWLC5)[^1].
I also have a pair of
[wireless headphones](http://www.amazon.com/gp/product/B003VANOFY/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B003VANOFY&linkCode=as2&tag=jakemccrary08-20&linkId=LDHRWNCUOO45B7G4)
but no longer use them. The wireless headphones were nice but the
over-ear design caused me to talk differently and at the end of the
day my throat would feel hoarse. The
[Snowball](http://www.amazon.com/gp/product/B002OO333Q/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B002OO333Q&linkCode=as2&tag=jakemccrary08-20&linkId=XVNYHVNXTAC3J2RD)
directional microphone seems to be the microphone of choice for
developers who prefer not to use headphones.

[^1]: This link and all others to Amazon in this post is an affiliate link.


## Soloing

Some developers do not want to pair program 100% of the time. This is
reasonable. If you are working an an environment that encourages near
100% pair programming there are still opportunities for solo.

One way to introduce solo time is to take your lunch at a different
time than your pair. This provides both of you with an opportunity to
do a bit of soloing.

Another way that short solo time happens is because of meetings and
interviews. It isn't uncommon for half of a pair to leave for a bit to
join a meeting, give an interview, or jump over to help out another
developer for a bit.

Soloing also happens as a result of uneven team numbers. A team of
five will always have someone solo unless a member is out for the day.
