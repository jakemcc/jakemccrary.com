---
layout: post
title: "Speeding up Magit with the native-comp branch of Emacs"
date: 2020-12-28 20:43:05 -0600
comments: true
published: false
description: Here is a summary of trying out the native-comp branch of Emacs and how it changed Magit's performance on a large git repository.
keywords: emacs, magit
categories: 
- emacs
---

In my last article, [Speeding up Magit](/blog/2020/11/14/speeding-up-magit/), I showed how removing elements from Magit's status buffer drastically reduces the time `magit-status` takes on a large repository (from 4 seconds to around 0.348 seconds).
In a [comment on r/emacs](https://www.reddit.com/r/emacs/comments/k3xfa1/speeding_up_magit/ge5o0e0/?utm_source=reddit&utm_medium=web2x&context=3), someone performed a test using the `native-comp` feature of Emacs and another person wondered how it performed on the repository I used in my last post.

This reddit thread was the first time I had heard of the `native-comp` feature.
This feature lives on the `feature/native-comp` branch of the Emacs repository and it compiles Elisp code into native code.
Many users have reported noticeable speed improvements using it.
The [offical development log](http://akrl.sdf.org/gccemacs.html) and [Emacs Wiki](https://www.emacswiki.org/emacs/GccEmacs) have more information about it.
I'll provide more information about getting `native-comp` on macOS after the section on Magit timings.

## How did it affect refresh times of the Magit status buffer?

The quick answer is that running Emacs with `native-comp` improved the refresh times of the Magit status buffer.
Below is a table of the various times.

```org

| Experiment                              | magit-status refesh time |
|-----------------------------------------+--------------------------|
| full magit-status with native-comp      | 3.152 seconds            |
| full magit-status without native-comp   | 4.003 seconds            |
| magit-status with many sections removed | 0.348 seconds            |
```

That is a pretty solid improvement.
It still isn't close to the time with many sections removed though, so I'll be sticking with my [Magit setup](/blog/2020/11/14/speeding-up-magit/) outlined in the previous post.

As a caveat, the timing with `native-comp` also includes upgrading Emacs from `26.3` to `28.0.50` (so I could have `native-comp`) and Magit was upgraded from `20201111.1436` to `20201212.929`.
As a result, the comparion to `full magit-status without native-comp` might not entirely fair as multiple variables have changed.
I'm still using `native-comp` and the refresh time with sections removed hasn't noticably changed.

## Getting `native-comp` on macOS

To have `native-comp` enabled you need to build Emacs from source.
I've done this before on Linux systems but this was the first time I've done this on macOS.

When browising reddit, I found the [build-emacs-for-macos](https://github.com/jimeh/build-emacs-for-macos) project which has some helpful instructions for doing this.
I followed the instructions from the readme and picked the latest known good commit from [this issue](https://github.com/jimeh/build-emacs-for-macos/issues/6) on the repo.

At the time I did this, the newest commit in that issue was `be907b0ba82c2a65e0468d50653cae8a7cf5f16b` when means I ran `./build-emacs-for-macos --git-sha be907b0ba82c2a65e0468d50653cae8a7cf5f16b feature/native-comp` to build Emacs.
I then updated my [init.el](https://github.com/jakemcc/emacs.d/commit/72cf37a497b72b8990956395e2eaa87285ea7c81) based on instructions from in the `build-emacs-for-macos` project.

I haven't had any issues since switching to it and waiting all my Elisp to compile.
I don't have numbers to back this up but it does feel faster.

## Recommendation

I'd recommend giving the `native-comp` feature of Emacs a shot.
It wasn't terribly challenging to get setup and it is nice to get a glimpse of what the future of Emacs might be.
That future is a bit snappier.
