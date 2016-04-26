---
layout: post
title: "Speeding up my blog"
date: 2016-04-22 19:34:05 -0500
comments: true
published: false
description: In an effort to improve my readers' experience I made my Octopress blog load significantly faster.
keywords: 'octopress, blog, writing, javascript'
categories: 
- blog
---

I was reading [Jeff Ramnani's](https://jefframnani.com/about/), a
former co-worker of mine, about page recently and I was somewhat
inspired by his _Colophon_ section. In it he links to Matt Gemmell's
[Designing blogs for readers](http://mattgemmell.com/designing-blogs-for-readers/)
article. Reading articles on Jeff's site and Gemmell's inspired me to
think about my own web site and what experience I want to deliver to
the reader.

Thinking more about my reader's experience made me think more about
what I like when read someone's writing. I like a clean, minimal
design (example: [zen habits](http://zenhabits.net/falling/)) and I
like it when a page loads fast. Basically I hate waiting and I don't
want to be distracted from the content.

I decided to focus on page speed first. As with any optimization
problem it is important to establish your starting point. To do so I
used [Web Page Test](http://www.webpagetest.org) and Google's
[PageSpeed Insights](https://developers.google.com/speed/pagespeed/insights/). Unfortunately
I didn't capture my starting point with Page Speed Insights but I did
capture my starting point using Web Page Test.

![Starting point from Web Page Test](/images/before-optimizations.png)

According to Web Page Test, the first load of my main page took five
seconds and it wasn't fully loaded for another second. This is
ridiculous. My page is nearly entirely all static content and most of
my assets are served from CloudFlare.

My first step was to look at what was actually being loaded. Google's
PageSpeed Insights identified that I had three render-blocking
`script` tags. The offending scripts were modernizr, jQuery, and
octopress.js. PageSpeed Insights
[recommends](https://developers.google.com/speed/docs/insights/BlockingJS#overview)
inlining small bits that you require to render the page or make
loading asynchronous. I decided to go a step further and remove the
need for the JavaScript.

To do this I started by looking at `octopress.js` to try to figure out
what functionality it
provided. [It](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js)
did a few things that I didn't care about at all (some sort of
[flash video fallback](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L111),
adding line numbers to
[GitHub Gists](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L112),
and
[rendering](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L99)
delicious links, and toggling the
[sidebar visibility](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L114))
and I was able to just delete that code. One feature that was useful
was it helped with
[mobile navigation](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L1-L13). In
order to delete that code I restyled the navigation bar to fit better
on small screens which removed the need for the navigation to turn
into a select field. `ocotpress.js` also did some
[feature detection](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L37)
for Modernizr. I stopped using image masks and was able to remove that
code as well. The only code left in `octopress.js` was to fix some
[iOS scaling bug](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L121-L136). This
I inlined into my html. At this point `octopress.js` was empty and
with it empty the requirements for jQuery and Modernizer
disappeared. This let me remove three render-blocking `script` tags.

A much easier win was to remove my recent tweets from the
sidebar. This was easy to do, simply stop loading JavaScript from
twitter, and removed the need to load yet another JavaScript file and
make a call out to Twitter for recent tweets.

After doing all of the above Google's PageSpeed Insights was just
complaining about short cache times for some of my assets and
complained about my one css file and two web fonts being loaded in my
`head` section. I fixed the short cache times by bumping the expire
time setting in CloudFlare. At this point I'm not worrying about the
fonts. My Web Page Test time is down to 2.5 seconds.

![Web Page Test after optimization](/images/after-optimizations.png)

Honestly, that number still seems slow to me but it is better than it
was and I have a greater understanding about what is going on with my
site. The speed improvement is good enough for now for me to move onto
spending my web site tweaking time on design (or time researching a
couple topics I've got brewing).
