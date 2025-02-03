---
dated-url: true
layout: post
title: "Fast feedback is important"
date: 2019-10-08 18:10:02 -0500
comments: true
published: false
description: PUT SUMMARY HERE
keywords: 'software'
categories:
- software
- philosophy
---

I was asked last year what I would focus on if I were leading a technology group.
My response was that I would focus on developing software with a focus on having fast feedback cycles.

Fast feedback cycles enable so much.

If you are working in a language that has a compile step and your compilation takes 100 milliseconds, you are going to feel free to compile all the time.
You'll get that feedback you've screwed something up super fast.
If you project takes 30 minutes to compile, you definitely will not be compiling all the time.
As a result, you'll write more code between compilation attempts.
More code written leads to more areas where there could be a problem.
Instead of getting feedback extremely close to when you first wrote a chunk of code, now you are getting it much later.
Getting the feedback much later puts a higher cognitive load on you as now you need to recall what you were attempting to do there and it has been minutes (or tens of minutes or an hour) since you've written that code.

When you are developing a new feature or fixing a bug having a quick feedback cycle from writing code to seeing tests pass or fail allows you to develop code faster with confidence.
As soon as that time to get feedback from running tests passes some threshold, all of a sudden you are not going to run tests all the time.
As a result you start writing more code between running your tests.
Now you have more areas where a problem could be introduced.

Lets make a diagram of the various steps of code through a developement lifecycle.

|Local Development| -> |Build Server| -> |Deploy to production| -> |Learn from deployed code|

Local Development is a series of

1) Write Code
2) Compile
3) Run tests
4) Push code to shared repository

Each step can cycle back to Step 1 (even Step 1).

Why is focusing on fast feedback cycles important?

Fast feedback cycles let you iterate on ideas faster. This lets you evolve quicker.

Slowness in any step lets distraction get it. It also encourages larger batches.

Larger batches of changes brings more risk.