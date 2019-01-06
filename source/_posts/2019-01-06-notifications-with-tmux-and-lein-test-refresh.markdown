---
layout: post
title: "Notifications with tmux and lein-test-refresh"
date: 2019-01-06 15:31:19 -0600
comments: true
published: false
description: This lein-test-refresh setting lets you run lein test-refresh in a non-visible tmux window and still see test results
keywords: 'lein-test-refresh, tmux'
categories: 
- lein-test-refresh
- tmux
- testing
- clojure
- tools
---

I've been doing more work from home recently and have found myself SSHing into my beefy Linux workstation in the office. While I can work on software on my MacBook Pro, I've found it more effective to SSH into my Linux workstation and fire up a terminal Emacs and Clojure repl. This lets me interact with the pieces of software that are trickier to get working locally under OS X.

When I'm working on a Clojure project, I like to have `lein test-refresh` always running somewhere. I don't often actually look at the output, I rely on notifications to tell me my tests are passing or failing.

Remitting into a machine thwarts my typical pop-up notifications so I've turned to tmux. Below is a GIF of the notifications I get as my tests run and pass or fail.

![tmux and test-refresh notifications](/images/tmux-test-refresh.gif "tmux and test-refresh notifications")

This is pretty easy to setup. You can trigger a message in tmux by running `tmux display-message <MESSAGE_HERE>`. To configure [lein-test-refresh](https://github.com/jakemcc/lein-test-refresh#notifications) to send notifications by tmux, update the `:test-refresh` section of your `project.clj` or `profiles.clj` to look like the following.

```clojure
:test-refresh {:notify-command ["tmux" "display-message"]}
```

Pretty straightforward change. Now I find myself toggling between that setting and my usual one that provides pop-up notifications depending on whether or not I'm local to my Linux box or SSHed in. A next step might be to introduce a shell script that does the right thing depending on if I'm in tmux or not.

I hope you enjoy this. Its has made using a remote terminal with tmux and (lein-test-refresh)[https://github.com/jakemcc/lein-test-refresh] more enjoyable.
