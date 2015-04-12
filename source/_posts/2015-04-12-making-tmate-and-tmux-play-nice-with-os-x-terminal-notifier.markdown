---
layout: post
title: Making tmate and tmux play nice with OS X terminal-notifier
date: 2015-04-12 17:47
comments: true
published: true
categories:
- tmux
- tmate
- osx
---

For nearly the last two years, I've been doing most of my development
in OS X. Most of that development has been done in Clojure and,
whenever possible, using
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh) with
[terminal-notifier](https://github.com/alloy/terminal-notifier) to
have my tests automatically run and a notification shown with the
status of the test run. Its a great work flow that gives me a quick
feedback cycle and doesn't pull my attention in different directions.

Recently I've started using [tmate](http://tmate.io/) for remote
pairing. Unfortunately when I first started using it my quick feedback
cycle was broken. `lein test-refresh` would run my tests but would
become hung when sending a notification using `terminal-notifier`.
This was terrible and, if I hadn't been able to fix it, would have
stopped me from using `tmate`. After some searching I stumbled across
[this](https://github.com/alloy/terminal-notifier/issues/115) GitHub
issue which helped solve the problem.

To make `tmate` work nicely with `terminal-notifier` you'll need
to install
[reattach-to-user-namespace](https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard)
and change your `tmate` configuration to use it. If you use `brew` you
can install by running `brew install --with-wrap-pbcopy-and-pbpaste
reattach-to-user-namespace`. Then open your `.tmux.conf` or
`.tmate.conf` file and add the line below.

```
set-option -g default-command "which reattach-to-user-namespace > /dev/null && reattach-to-user-namespace -l $SHELL || $SHELL"
```

The above tells `tmate` to use `reattach-to-user-namespace` if it is
available. Now `terminal-notifier` no longer hangs when invoked inside
`tmate`. Unsurprisingly, this change also makes `tmux` place nice with
`terminal-notifier`.
