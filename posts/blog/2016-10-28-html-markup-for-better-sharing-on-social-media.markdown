---
dated-url: true
layout: post
title: HTML markup for better sharing on social media
date: 2016-10-28 10:05 -0500
comments: true
published: true
description: Short, no nonsense guide to improving how your articles appear when shared
  on social media.
keywords: Facebook, HTML, markup, social media, twitter
categories: 
- blog
---

For a bit more than a year I worked on a project that crawled the web
and indexed articles. Two of our sources of data were articles shared
on Facebook and Twitter. After seeing hundreds of article previews on
these two social networks, I decided to improve how my own articles were
previewed.

I thought figuring out the markup I needed to add would be a painless
experience. Unfortunately, when you search for this information you
end up at various SEO optimization and other similar sites where you
get the pleasure of experiencing full screen pop-overs trying to get
you to sign up for mailing lists and other annoying features of the
modern web. Probably unsurprisingly, the least annoying source for
this information turned out to be the social networks themselves.

Below is what you will want to add to the `<head>` section of
your articles' markup. Items in all caps should be values that make
sense for your articles. Most fields are pretty self-evident, but
check [Twitter's](https://dev.twitter.com/cards/markup)
and
[Facebook's](https://developers.facebook.com/docs/sharing/webmasters#markup) documentation
for more details. The [Open Graph](http://ogp.me/) documentation has
more details as well.

```html
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

If you have control of your site's markup and want better previews of
your articles on the various social networks then you should add this
markup to your web site [^1]. Hopefully this article has saved you
from having a full screen pop-over prompt you to join yet another
mailing list.

[^1]: You can actually remove the `twitter:title`, `twitter:description`, and `twitter:image` lines since Twitter will fallback to the equivalent Open Graph markup if they missing.
