---
layout: post
title: "Use Google to get a site's favicon"
date: 2016-05-11 16:22:58 -0500
comments: true
published: false
description: It is easy to use a feature from Google to get a site's favicon.
keywords: favicon
categories: 
- web
---

A few months ago I was implementing some changes to
[Lumanu's](https://lumanu.com) user interface. Lumanu is a tool I've
been working on that helps its users create, discover, and curate
engaging content.

This interface change was to our discovery view. This is the view that
surfaces interesting content to our users. The change involved
showing the favicon of content's origin in our interface.

I often browse the Internet with the network tab in Chrome Developer
Tools open. I think it can be interesting to see what services other
web applications are using. I had the network tab open while browsing
a site that displayed many favicons and noticed a lot fetches from
google.com. This surprised me so I took a deeper look at the requests
and saw they were hitting a url that seemed to provide favicons. It
turns out you can query Google for favicons.

Let's pretend we want to get the favicon for `jakemccrary.com`. You
simply construct a URL that looks like
[`https://www.google.com/s2/favicons?domain=jakemccrary.com`](https://www.google.com/s2/favicons?domain=jakemccrary.com)
and all of a sudden you have the favicon. If you replace
`jakemccrary.com` with the domain you care about and you'll be
rewarded with that domain's favicon.

![My favicon from Google](https://www.google.com/s2/favicons?domain=jakemccrary.com "Favicon from Google")

This definitely isn't a new feature. If you search online you'll see
poeple talking about it years ago. I had never heard of it before and
discovering it saved us an unknown amount of time. It allowed us to
iterate on our interface without having to figure out the nuances of
favicons. This feature allowed us to quickly implement the interface
idea and then throw it away without costing us too much time.
