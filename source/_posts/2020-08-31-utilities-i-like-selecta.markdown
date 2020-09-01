---
layout: post
title: "Utilities I like: selecta"
date: 2020-08-31 21:00:09 -0500
comments: true
published: false
description: Selecta is a useful 
keywords: 'selecta, utility, command line'
categories: 
- utilities
- command-line
- linux
- macos
- tools
---

[Selecta](https://github.com/garybernhardt/selecta) is a command line utility that gives you the power to fuzzy search items in a list of text.
What does that mean?
It means you pipe it a list of text on stdin, it helps you make a choice, and then prints that choice to stdout.

Here is an example of me using it to help me narrow in on what file I'd like to pass to `wc`.

![Example of using selecta](/images/selecta-search.gif)

That example isn't the greatest use of `selecta` but hopefully it helps you see how it can be used.

About five years ago, I used `selecta` to build a script called `connect-db`.
This script parsed my `~/.pgpass` file using `grep`, `sed`, and `cut` with `selecta` to provide a command line fuzzy searcher for quickly connecting to known databases.
It was a very useful script.

I've also seen tools similar to `selecta`, such as [fzf](https://github.com/junegunn/fzf), used to build helpful tools for running subsets of tests quicker.

By combining `selecta` with the various Unix tools that use stdin and stdout to pass lists of text around you can build pretty friendly tools.
It is a useful utility to add to our toolkit.

