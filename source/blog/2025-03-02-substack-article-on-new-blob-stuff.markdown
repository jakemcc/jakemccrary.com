---
layout: post
title: substack article on new blob stuff
date: 2025-03-02 12:04 -0600
comments: true
published: false
description: TODO
categories:
- TODO
---

Hello readers,

By this time, you've probably forgotten that you have signed up to receive the occaisonal email from me.
Oops, I haven't been writing much at http://jakemccrary.com and this newsletter mostly exists because folks wanted to get emails when I write new articles over there.

Well, there has been a fairly large site redesign and articles published over there since my last email.
Here is a summary.

## http://jakemccrary.com redesign

My old setup that used an ancient version of jekyll with Octopress enhancements stopped generating.
Instead of fixing that, which would have taken many fewer hours, I opted to rewrite the site generation using Babashka, a Clojure dialect so that I can understand every level of the site generation and, mostly, only blame myself when it breaks in the future.

I'm sure the design will continue to evolve, especially the main page.
It continues to load much, much faster than most websites out there.

## New articles since last email

### [Bookmarklets on mobile are useful](https://jakemccrary.com/blog/2022/11/13/bookmarklets-on-mobile-are-useful/)

The archive.is bookmarket mentioned in this article is something I use almost daily on my mobile device.
It is super useful.

### [Reading in 2022](https://jakemccrary.com/blog/2023/01/14/reading-in-2022/), [Reading in 2023](https://jakemccrary.com/blog/2024/02/18/reading-in-2023/), and [Reading in 2024](https://jakemccrary.com/blog/reading-in-2024/)

I've continued to read great books.
In the last few years I've discovered the author Miranda July and her writing really jives with me.

### [Scheduling cron tasks in mixed time zones](https://jakemccrary.com/blog/2024/06/16/scheduling-cron-tasks-in-mixed-time-zones/)

My team uses boring technology (except for Clojure).
From that, it follows that we use cron to schedule stopping and starting hundreds of services through out our work week.

We also have to deal with a variety of timezones.
Figuring out how to have cron work with timezones has drasitcally cut down on the amount of risk around day light savings time shifts.
Linux experts told me this couldn't be done and the internet wasn't super useful figuring out how to do this.
Luckily, there is always the source code.

### [A couple quality of life improving functions added to emacs](https://jakemccrary.com/blog/a-couple-tiny-elisp-functions-for-improving-living-in-emacs/)

A short post showing off a couple of quality of life improvements I've made to my emacs setup over the last year or so.
LLMs drastically lower the bar to writing these types of small, minor annoyance removing functions.

## End

The next email will come whenever I've written more on jakemccrary.com.
It will probably get back to the previous format of discussing some books I've been reading as well and articles I've enjoyed across the internet.

Till next time,
Jake