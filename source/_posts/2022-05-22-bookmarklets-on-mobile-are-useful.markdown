---
layout: post
title: "Bookmarklets on mobile are useful"
date: 2022-05-22 16:00:25 -0500
comments: true
published: false
description: PUT SUMMARY HERE 
keywords: 'csv, keywords, here'
categories: 
---

I do a lot of article reading on my phone.
This is convenient but a couple tricks I've adopted for reading on a computer web browser are much harder to do on a phone.
These tricks involved editing the URL in the location bar and the way mobile browsers location and keyboard interact make it much trickier.

The tricks I do are for getting around various paywalls.

The first trick is specific to The Atlantic.
At The Atlantic, if you put a `.` after the `.com` then you are able to go past your monthly article limit.
You can try for yourself and see how much easier this is with your computer browser compared to your mobile browser.

The second trick is to prefix a url with `http://archive.is/`.
This uses the archive.is service to both archive the url (if not already archived) and view the archived content.
This has the benefit of getting around many other paywalls and archives the site so you can find the exact content later.
Also, annoying to do on a mobile browser.

Luickly, bookmarklets work on mobile browsers.
For those that don't know, bookmarklets are little snippets of javascript that you put into a brower's bookmark.
Then when you select that bookmark the javascript executes.

On mobile Chrome and Brave, you can execute a bookmark by searching for its name in your location bar.
This discovery, found by reading this article TODO, encouraged me to write two bookmarkets.
One called OpenArchive and another called FixAtlantic.

``` javascript
javascript:(function() {window.location="http://archive.is/"+window.location.toString();}())
```

``` javascript
javascript:(function() {window.location=window.location.href.replace(/theatlantic.com/, 'theatlantic.com.');}())
```

To get them onto my phone, I added them a bookmarks on my laptop's Chrome and synced them to my mobile phone.
Once in my mobile Chrome, I copied the code and pasted it into a bookmark in Brave.

These bookmarklets are great.
I've only had them for a couple days but they are already improving my mobile experience.
