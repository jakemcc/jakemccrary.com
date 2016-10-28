---
layout: post
title: "HTML markup for better sharing on social media"
date: 2016-10-28 08:59:51 -0500
comments: true
published: false
description: Short, no nonsense guide to improving how your articles appear when shared on social media.
keywords: 'Facebook, HTML, markup, social media, twitter'
categories: blog
---

For a bit more than a year I worked on a project that involved web
crawling and consuming status updates Facebook and Twitter. After
seeing thousands of posts on social media I finally took the time to
improve this blogs markup so that articles would be previewed better
when shared on Facebook and Twitter.

It took a bit of digging to figure out what markup needed to be
included with each post. Most of the articles you find are from scammy
feeling SEO blogs or other equally scammy feeling "optimize your
presence on social media" articles. The useful information in those
articles is buried under pop-ups trying to get you onto a mailing list
and other annoying features of the modern web.

Below is the markup you'll want to add to your articles to have them
preview better when shared on Facebook and Twitter. I'm just going to
copy this from my template that is used to generate the `<head>`
section of my articles. Items in all caps should be values that make
sense for your articles. Check
the [Twitter](https://dev.twitter.com/cards/markup)
and
[Facebook](https://developers.facebook.com/docs/sharing/webmasters#markup). The
[Open Graph](http://ogp.me/) documentation has more details on the
Open Graph markup.

```html
<meta itemprop="name" content="ARTICLE TITLE" />
<meta itemprop="description" content="SHORT DESCRIPTION OF CONTENT" />

<!-- Twitter Card data -->
<meta name="twitter:card" content="SUMMARY" />
<meta name="twitter:site" content="TWITTER HANDLE OF SITE (@jakemcc for this site)" />
<meta name="twitter:creator" content="YOUR TWITTER HANDLE" />
<meta name="twitter:title" content="ARTICLE TITLE" />
<meta name="twitter:description" content="SHORT DESCRIPTION OF CONTENT" />
<meta name="twitter:image" content="IMAGE THAT SHOWS UP WITH PREVIEW" />

<!-- Open Graph data -->
<meta property="og:site_name" content="SITE TITLE" />
<meta property="og:url" content="CANONICAL URL" />
<meta property="og:title" content="ARTICLE TITLE" />
<meta property="og:description" content="SHORT DESCRIPTION OF CONTENT" />
<meta property="og:image" content="IMAGE THAT SHOWS UP WITH PREVIEW" />
<meta property="og:type" content="article" />
<meta property="article:published_time" content="PUBLISHED DATETIME" />
```

There you go. If you're in control of your site's markup and want to
start having the preview of your articles show up better on various
social media outlets then you should add similar markup to your web
site  [^1]. Hopefully this article has saved you from having some full
screen pop-over prompt you to join yet another mailing list.

[^1]: You can actually remove the `twitter:title`, `twitter:description`, and `twitter:image` lines since Twitter will fallback to the equivalent Open Graph markup if they missing.
