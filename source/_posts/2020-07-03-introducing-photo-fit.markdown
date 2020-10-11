---
layout: post
title: Introducing Photo Fit
date: 2020-07-03 18:37 -0500
comments: true
published: true
description: Introducing Photo Fit, a TypeScript web app that makes your landscape
  photos work well with your vertical phone.
keywords: resize photo, typescript, javascript, photo, landscape, portrait
categories:
- typescript
- experiment
---

Earlier this year, I wanted to use a landscape photo as my background on my phone.
It wasn't the photo below but we can use it as an example.

{% img center /images/photo-fit/keyboard-new-keycaps.jpg "Landscape image of my keyboard" "Landscape image of my keyboard" %}

When I made it my background, my phone[^1] zoomed in to make it fit the portrait orientation of the phone.

[^1]: A Samsung S8 running Android 9 

{% img center /images/photo-fit/phone-background-before.jpg "Screenshot of phone with zoomed in keyboard photo" "Screenshot of phone with zoomed in keyboard photo" %}

This is not great.
I don't want a zoomed in version that fits my vertical phone.
I want to see the whole photo with black bars at the top and bottom

I tried to find a way to add these bars using my phone.
I couldn't find an easy way.

At this point, a reasonable solution would have been transferring the photo to a computer, editing it, and transferring it back to my phone.
I didn't do that.
Instead, I wrote a little TypeScript[^2] web app that adds the bars for you.
You open the website on your phone, select an image, and then download a properly sized image.

{% img center /images/photo-fit/phone-background-after.jpg "Screenshot of phone with properly fitting image" "Screenshot of phone with properly fitting image" %}

The tool uses the canvas API and does all of the work in the browser itself.
It was a fun, bite-sized project and it gave me an excuse to write some TypeScript and do some web programming.
This was the first time I've written TypeScript since learning it and I haven't done any web programming in a while.

Making [Photo Fit](/experiments/photo-fit/) was not a fast approach to changing my phone's background.
But, now the tool exists and anyone, including future me, can quickly resize their photo from the comfort of their own phone.

[Photo Fit](/experiments/photo-fit/) is live and available for others to use.
I've only tested it on my own phone and desktop browsers.
It might not work!
If you do try it and something weird happens, plese let me know.

[^2]: I recently learned some TypeScript through [Execute Program](https://www.executeprogram.com). Execute program is a really neat application of spaced repetition for learning programming concepts.

