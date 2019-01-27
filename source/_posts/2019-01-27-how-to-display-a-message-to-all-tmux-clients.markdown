---
layout: post
title: How to display a message to all tmux clients
date: 2019-01-27 17:03 -0600
comments: true
published: true
description: Here is how to display a message to all tmux clients
image: images/tmux-notify-script.gif
keywords: tmux
categories:
- tmux
- tools
---

<!-- Estimated time: 30 minutes -->
<!-- First Draft: 26 minutes -->
<!-- First edits: 14 minutes -->

Lately, I've been using [tmux](https://github.com/tmux/tmux) a lot. This resulted in me figuring out how to get [lein-test-refresh](https://github.com/jakemcc/lein-test-refresh#notifications) to send [notifications using tmux](/blog/2019/01/06/notifications-with-tmux-and-lein-test-refresh/).

The setup linked above works great for when I'm doing work all by myself. It showed a problem when using ssh and tmux to pair with another developer. Instead of both developers receiving a notification, only one did. One is better than none but not ideal.

Below is a GIF showing the problem. Each window simulates a different developer.

![tmux only showing one developer a notification](/images/tmux-pair-fail.gif)

This wasn't too hard to fix. A little digging through the tmux manpage shows that `tmux display-message` takes an optional flag telling it which client receives the message. If we can get a list of all the clients then iterating over them and sending a message to each is straightforward.

`tmux list-clients` give us this list. Below is the output.

```
$ tmux list-clients
/dev/ttys002: 0 [78x41 xterm-256color] (utf8)
/dev/ttys006: 0 [78x42 xterm-256color] (utf8)
```

What we care about are the parts that look like `/dev/ttys002`. At first I used `cut` to grab these values but then I dug a bit deeper into the `tmux` manpage.

It turns out that you can specify a format to `tmux list-clients`. Running `tmux list-clients -F "#{client_name}"` gives us the output we care about.

```
$ tmux list-clients -F "#{client_name}"
/dev/ttys002
/dev/ttys006
```

We can combine that with `xargs` to send a message to multiple clients.

![tmux xargs example](/images/tmux-xargs-example.gif)

That command is a bit much to put into `lein-test-refresh`'s configuration so I shoved it in a script called `notify` and configured `lein-test-refresh` to use it. Script and GIF of that below. Now both you and your pair can get notifications.

```bash
#!/bin/bash

USAGE="Usage: notify <message>

example: notify 'Tests passed!'"

if [ -z "$1" ]; then
    echo "$USAGE"
    exit 1
fi

message="$1"

tmux list-clients -F "#{client_name}" \
    | xargs -n1 -I{} tmux display-message -c {} "$message"
```

![Example using notify script](/images/tmux-notify-script.gif)


