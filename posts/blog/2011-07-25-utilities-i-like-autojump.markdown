---
comments: true
layout: post
title: "Utilities I like: autojump"
categories: [utilities, command-line, linux, tools]
date: "2011-07-25"
---

[autojump](https://github.com/joelthelion/autojump) is a nifty command line tool that enables quicker jumping between directories. I've been using it for a few months now and miss it when I work other machines.

To jump to a directory you type `j SUBSTRING_OF_DIR`. Example:

``` bash
    $ pwd
    /Users/jmccrary
    $ j jake
    /Users/jmccrary/src/github/jakemcc/jakemccrary.com
    $ pwd
    /Users/jmccrary/src/github/jakemcc/jakemccrary.com
```

Above I jumped from my home directory to the root of this website's code. Being able to jump between directories by just remembering a name (or part of a name) is great. This frees me from having to remember full paths or set up aliases.

autojump works by keeping a database of "time" spent in directories and jumps to the most frequently visited one that match `SUBSTRING_OF_DIR`. If you are curious as to what that database looks like the tool `jumpstat` will give you a view.

I used to set up aliases for jumping between projects but now that I've trained myself to use autojump I don't think I'll ever go back. Not having to do any extra work besides simply entering the root directory of new projects to setup efficient directory movement is great. I think that if you give it a shot for a while you'll find the same benefits.

If you are on a Mac and use [homebrew](https://github.com/mxcl/homebrew) you can install by doing `brew install autojump`. For other platforms check out the [github](https://github.com/joelthelion/autojump) page.

