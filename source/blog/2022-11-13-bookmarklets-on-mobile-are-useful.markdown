---
dated-url: true
layout: post
title: Bookmarklets on mobile are useful
date: 2022-11-13 18:36 -0600
comments: true
published: true
description: Triggering bookmarklets in your mobile web browser doesn't have to be painful.
keywords: bookmarklet, mobile, browser
categories:
- mobile
- browser
- javascript
---

Bookmarklets, little snippets of JavaScript that you keep around as a bookmark, are useful.
They let you execute some JavaScript to perform almost any action you want on a website.

Some bookmarklets I use on my desktop browser include:

1. A collection of bookmarklets that let you change the playback speed of most embedded videos.
1. A bookmarklet to manipulate the URL of the page you're visiting.
1. A [bookmarklet](https://pinboard.in/howto/) to save the current page's URL to pinboard.in.

For years, I thought I was restricted to only using bookmarklets in my desktop web browser.
I hadn't effectively used mobile bookmarks before and thought that clicking them would be a huge pain.

It turns out, I was wrong!
I recently learned that if you start typing a bookmark's title into your mobile browser's location bar, it will let you select the bookmark.
This means you can easily execute a bookmarklet just by starting to type its name and clicking it when it appears.
This "search for bookmark in location bar" technique works with at least Google Chrome and Brave running in Android.

Below are the two bookmarklets I use regularly on my phone.
They exist to bypass paywalls.

This one prepends `http://archive.is/` to the current URL:

``` javascript
javascript:(function() {window.location="http://archive.is/"+window.location.toString();}())
```

This one changes `theatlantic.com` to `theatlantic.com.` (though it no longer gets around their paywall):

``` javascript
javascript:(function() {window.location=window.location.href.replace(/theatlantic.com/, 'theatlantic.com.');}())
```

To get them onto my phone, I added them a bookmarks on my laptop's Chrome and synced them to my mobile phone.
Once in my mobile Chrome, I edited the bookmark in mobile Chrome, copied the code, and pasted it into a bookmark in Brave.

I type three characters into my mobile browser's location bar before I can select either of these bookmarklets.
That is quicker than editing the URLs by hand and has improved the experience of reading articles on my phone.