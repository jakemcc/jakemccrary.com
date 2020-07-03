---
layout: post
title: "Introducing Photo Fit"
date: 2020-05-10 20:59:05 -0500
comments: true
published: false
description: Introducing Photo Fit, a TypeScript web app that makes your landscape photos work well with your vertical phone.
keywords: 'csv, keywords, here'
categories: 
---

Earlier this year, I wanted to make a photo the new background on my phone.
The photo was taken in landscape mode on my phone, similar to the photo of my keyboard below.

{% img center /images/photo-fit/keyboard-new-keycaps.jpg "Landscape image of my keyboard" "Landscape image of my keyboard" %}

When I made it my background, my phone[^1] zoomed in to make it fit the portrait orientation of the phone.

[^1]: A Samsung S8 running Android 9 

{% img center /images/photo-fit/phone-background-before.jpg "Screenshot of phone with zoomed in keyboard photo" "Screenshot of phone with zoomed in keyboard photo" %}

This is not great.
I don't want a zoomed in version of the photo that fits my vertical phone.
I want to see the whole photo with black bars at the top and bottom

I tried to find a way to add these bars using my phone.
I couldn't find an easy way.

At this point, a reasonable action would be to transfer the photo to a computer, edit it, and transfer it back to my phone.
I didn't do that.
Instead, I wrote a tiny little TypeScript[^2] web app that adds the bars for you.
You open the website on your phone, select an image, and then download a properly sized image.

{% img center /images/photo-fit/phone-background-after.jpg "Screenshot of phone with properly fitting image" "Screenshot of phone with properly fitting image" %}

The tool uses the canvas API and does all of the work in the browser itself.
It was a fun, bite-sized project and it gave me an excuse to write some TypeScript and do some web programming.
I haven't written much JavaScript in the last year and this was the first TypeScript I've written since I first learned it.

This was definitely the long approach to getting the image as my background.
But, now the tool exists and the next time I, or anyone else, needs to do this it will take all of twenty seconds.

[^2]: I recently learned some TypeScript through [Execute Program](https:www.executeprogram.com). Execute program is a really need application of spaced repetition for learning programming concepts.

