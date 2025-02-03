---
dated-url: true
layout: post
title: Go create silly, small programs
date: 2020-10-03 13:24 -0500
comments: true
published: true
description: Solving small, sort of silly problems with software can make tiny improvements
  to the world. Go do it.
keywords: programming, problems, typescript, strava, onewheel, heroku
categories:
- programming
- inspiration
- typescript
---

Over the summer, I developed a couple of small, sort of silly programs.
One, [Photo Fit](https://jakemccrary.com/experiments/photo-fit/), is a little tool that runs in a web browser and resizes photos to fit as your phone's background.
The other, [Default Equipment](https://default-equipment.herokuapp.com/), runs on Heroku and automates changing the "bike" of my Strava-tracked e-bike rides to be my [onewheel](https://onewheel.com/).

These weren't created to solve large problems in the world.
There is no plan to make any money with them.
As of October 2020, Default Equipment doesn't even work for other people (though it could, send me a message if you'd like to use it and I'll get around to it).

Each was created to fix a minor annoyance in my life and, because these tools can live on the Internet, they can fix the same minor annoyance in other lives.

With an increasing amount of software in the world, being able to write software is nearly sorcery[^1].
As a developer, you can identify a problem in the world and then change the world to remove that problem.
And, depending on the problem, you can remove it for everyone else.

Software developers aren't alone in being able to identify problems and remove them through creation.
Carpenters can build shelves for their books.
Cooks can prepare food to remove hunger.
You can come up with nearly an infinite number of other examples.

The difference is that a solo developer can solve problems for an unknown number of other folks.
This is enabled by the Internet enabled ease of distribution.
This is very powerful.

Developers can expose their solution to others through a web application.
Desktop or mobile applications can be distributed through various app stores or made available as a download.
Source code can be made available for others to run.
Being able to distribute easily and cheaply is a game changer.

A developer's change to the world might be a minor improvement.
Photo Fit might never be used by anyone besides me.
But it is still out there, making the world slightly better.
It is available for someone to stumble upon when they are also annoyed by the same problem.

It felt good to write these tiny, useful programs.
If you scope them small enough, there is a definitive ending point[^2].
This lets you feel that finishing-a-project satisfaction quickly.
The small size also allows you experiment with new techniques and tools without committing to a large and ongoing commitment.

I wrote both Photo Fit and Default Equipment in TypeScript.
Before the beginning of summer, I didn't know TypeScript and had little exposure to Node.js.
Now I have some experience with both and gained that while making small improvements to my life and potentially the lives of others.

If you haven't developed software to solve a small problem recently, I'd recommend doing it.
Don't hesitate to remove a problem that feels silly.
Removing those problems can still make your life slightly better and gives you an opportunity to learn.
It feels good to remove an annoyance from your life.
If you can, make that software available to others so their lives are improved as well.
Take advantage of the power of easy distribution to improve the world and not just your tiny slice of it.

[^1]: This is taken to an extreme in the fantasy series [Magic 2.0](https://www.goodreads.com/series/131379-magic-2-0).

[^2]: Excluding any ongoing maintenance. But if you're making something small enough you can approach near zero ongoing maintenance. One of my longest running solve-my-own-problems application, Book Robot, has been operating for nearly 7 years with minimal effort.