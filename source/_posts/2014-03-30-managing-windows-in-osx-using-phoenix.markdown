---
layout: post
title: "Managing windows in OS X using Phoenix"
date: 2014-03-29 17:36
comments: true
publish: false
categories: [osx, tools, phoenix]
---

[Last year](http://jakemccrary.com/blog/2013/04/15/manage-your-workspace-with-grids-under-linux-osx-and-windows/)
I wrote about how I managed my windows under OS X, Windows, and Linux.
I'm a big fan of having an orderly layout and try to use grid
managers. Since the last post on window managers I've changed jobs and
now my main machine is an MacBook Pro running OS X Mavericks with two
27 inch cinema displays. As a result I've started experimenting with
more OS X window managers. After trying a few out I'm going to stick
with [Phoenix](https://github.com/sdegutis/Phoenix).

## Before Phoenix ##

Last year I was satisfied using [Spectacle](http://spectacleapp.com/).
It is (or at least was, haven't used it in a while) easy to install
and had good defaults. I'd still recommend it for most people.

At the recommendation from a reader I switched to
[Slate](https://github.com/jigish/slate). Slate has a ton of features
and I barely scratched the surface in how I used it. I really just
used it as a replacement for Spectacle and didn't use any of the
advanced features. I ended up becoming dissatisfied with Slate. After
running for a while (talking at least a week) it started to respond
slowly to commands. I'd try to move a window to another monitor and it
wouldn't register. Eventually I'd be in another process and the
command would register and a different window would jump monitors.

## Phoenix ##

I was looking for solutions to Slate's unresponsiveness when I stumbled
on [Phoenix](https://github.com/sdegutis/Phoenix). I was drawn towards
the stated goal of stating that it "aims for efficiency and a very
small footprint." The fact that it is still being actively developed
was also a huge selling point.

Phoenix provides a
[JavaScript API](https://github.com/sdegutis/Phoenix/wiki/JavaScript-API-documentation)
that allows you to interact with your running applications or launch
applications. It doesn't provide anything out of the box; it is up to
you to make it useful by writing your own (or taking another persons)
configuration that adds the functionality you want.

This is a double-edged sword. This means you get exactly the features
you want. It also means you might spend significant amounts of time
figuring out how to get the features you want.

Luckily there are now
[examples](https://github.com/sdegutis/Phoenix/wiki) out there you can
follow in order to get the features you want. Browsing through those
should give a good starting point and idea of what types of features
are implementable.

My
[configuration](https://github.com/jakemcc/dotfiles/blob/master/phoenix.js)
is relatively minimal. I've written
[code](https://github.com/jakemcc/dotfiles/blob/master/phoenix.js#L42-L87)
to I have written code to move windows between monitors (rotating
between three added some complexity to this),
[start or focus](https://github.com/jakemcc/dotfiles/blob/master/phoenix.js#L90-L122)
certain applications, and
[resize](https://github.com/jakemcc/dotfiles/blob/master/phoenix.js#L11-L37)
windows. This is enough for me to feel efficient.

I encourage you to use a tool to help manage your windows. Personally
I think Phoenix is pretty great and don't mind tinkering. As a bonus
it is a young project where the maintainer is open to suggestions.
