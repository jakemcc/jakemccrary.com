---
layout: post
title: 'Utilities I like: selecta'
date: 2020-08-31 22:24 -0500
comments: true
published: true
description: Selecta is a useful command-line fuzzy text selector.
keywords: selecta, utility, command-line
categories:
- utilities
- command-line
- linux
- macos
- tools
---

[Selecta](https://github.com/garybernhardt/selecta) is a command-line utility that gives you the power to fuzzy select items from a list of text.
What does that mean?
It means you pipe `selecta` a list of text on stdin, it helps you make a choice from items in that list, and then `selecta` prints that choice to stdout.

Here is an example of me using it to help me narrow in on what file I'd like to pass to `wc`.

<video autoplay loop muted playsinline>
  <source src="/images/selecta-search.webm" type="video/webm">
  <source src="/images/selecta-search.mp4" type="video/mp4">
</video>

In this example, I search for markdown files using `ripgrep` (`rg`), type part of a filename, hit enter to select the match, and then see the `wc` stats of that file.
This isn't the greatest example of using `selecta` but it adequately shows what it does.

Some number of years ago, I wrote a script called `connect-db`.
This script used `selecta`, along with `grep`, `sed`, and `cut`, to provide a very pleasant command-line experience for connecting to known databases.
My coworkers and I used this script frequently.

By combining `selecta` with other stdin/stdout friendly command-line tools you can build really enjoyable, time-saving tools.
[Selecta](https://github.com/garybernhardt/selecta) is a useful utility to add to your toolkit.

