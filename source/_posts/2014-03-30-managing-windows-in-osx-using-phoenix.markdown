---
layout: post
title: "Managing windows in OS X using Phoenix"
date: 2014-03-30 18:30
comments: true
categories: [osx, tools, phoenix]
description: I manage my OS X windows efficiently using Phoenix and you can too.
---

[Last year](http://jakemccrary.com/blog/2013/04/15/manage-your-workspace-with-grids-under-linux-osx-and-windows/)
I wrote about how I managed my windows under OS X, Windows, and Linux.
I'm a big fan of having an orderly layout and try to use grid
managers. Since then I've changed jobs and now my main machine is an
MacBook Pro running OS X Mavericks with two 27 inch cinema displays.
As a result I've started experimenting with more OS X window managers.
After trying a few out I'm going to stick with
[Phoenix](https://github.com/sdegutis/Phoenix).

### Before Phoenix ###

Last year I was satisfied using [Spectacle](http://spectacleapp.com/).
It is (or at least was, I haven't used it in a while) easy to install
and had good defaults. I'd still recommend it for most people.

At the recommendation from a reader, I switched to
[Slate](https://github.com/jigish/slate). Slate has a ton of features
and I barely scratched the surface in how I used it. I used it as a
replacement for Spectacle and didn't touch any of the advanced
features. Before I had the urge to explore the advanced features I
ended up becoming dissatisfied with Slate. I ran into an issue where
after running for a while (talking at least a week) it would start to
respond slowly. I'd try to move a window to another monitor and it
wouldn't move. Eventually I'd be in another process and the
command would register sending whatever window I was currently focused
on to another monitor.

### Introducing Phoenix ###

I was looking for solutions to Slate's unresponsiveness when I
stumbled on [Phoenix](https://github.com/sdegutis/Phoenix). I was
drawn in by its stated goal; it "aims for efficiency and a very small
footprint." The fact that it is still being actively developed was
also a huge selling point. Knowing that any bugs I find have a
potential to be fixed is great.

Phoenix provides a
[JavaScript API](https://github.com/sdegutis/Phoenix/wiki/JavaScript-API-documentation)
that allows you to interact with your running applications or launch
applications. It doesn't provide anything out of the box; it is up to
you to make it useful by writing your own (or taking another persons)
configuration.

This is a double-edged sword. This means you get exactly the features
you want. It also means you might spend significant amounts of time
figuring out how to get the features you want.

Luckily there are [examples](https://github.com/sdegutis/Phoenix/wiki)
that you can use as a starting point. Browsing through the examples is
a great way of becoming familiar with what is possible and can be
inspiring.

My
[configuration](https://github.com/jakemcc/dotfiles/blob/17a73f89a2f3f7b2c9aa07a63d1928b86cc5425d/home/.phoenix.js)
is relatively minimal. I've written
[code](https://github.com/jakemcc/dotfiles/blob/17a73f89a2f3f7b2c9aa07a63d1928b86cc5425d/home/.phoenix.js#L42-L87)
to move windows between monitors (rotating between three added some
complexity to this),
[start or focus](https://github.com/jakemcc/dotfiles/blob/17a73f89a2f3f7b2c9aa07a63d1928b86cc5425d/home/.phoenix.js#L90-L122)
certain applications, and
[resize](https://github.com/jakemcc/dotfiles/blob/17a73f89a2f3f7b2c9aa07a63d1928b86cc5425d/home/.phoenix.js#L11-L37)
windows. This is enough for me to feel efficient.

I encourage you to use a tool to help manage your windows. Personally
I think Phoenix is pretty great and don't mind tinkering with my
configuration and strongly recommend it. As a bonus it is a young
project where the maintainer is open to suggestions. If you have an
idea for a useful
[feature](https://github.com/sdegutis/Phoenix/issues/18) it has a
possibility of being added pretty quickly.
