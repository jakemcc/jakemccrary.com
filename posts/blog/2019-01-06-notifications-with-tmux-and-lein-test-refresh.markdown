---
dated-url: true
layout: post
title: Notifications with tmux and lein-test-refresh
date: 2019-01-06 16:55 -0600
comments: true
published: true
description: This lein-test-refresh setting lets you run lein test-refresh in a non-visible
  tmux window and still see test results
image: images/tmux-test-refresh.gif
keywords: lein-test-refresh, tmux, clojure, testing, feedback
categories:
- lein-test-refresh
- tmux
- testing
- clojure
- tools
---

I've been using Emacs in a remote [tmux](https://github.com/tmux/tmux) session lately and I've been missing [lein-test-refresh](https://github.com/jakemcc/lein-test-refresh#notifications) notifications when my Clojure tests pass or fail. Luckily, it only took me a little bit of searching to figure out a solution for when I'm working inside of tmux.

Below is a GIF of the notifications I get as my tests run and pass or fail.

![tmux and test-refresh notifications](/images/tmux-test-refresh.gif "tmux and test-refresh notifications")

With the above notifications, I can keep my focus on my code and only switch to the tmux window with `lein test-refresh` running when a test fails.

This was pretty easy to setup. You can trigger a message in tmux by running `tmux display-message <MESSAGE_HERE>`. To configure [lein-test-refresh](https://github.com/jakemcc/lein-test-refresh#notifications) to send notifications to tmux simply include the following in your `:test-refresh` section of your `project.clj` or `profiles.clj`.

```clojure
:test-refresh {:notify-command ["tmux" "display-message"]}
```

I hope you enjoy this. Its has made using a remote terminal with tmux and [lein-test-refresh](https://github.com/jakemcc/lein-test-refresh) more enjoyable.