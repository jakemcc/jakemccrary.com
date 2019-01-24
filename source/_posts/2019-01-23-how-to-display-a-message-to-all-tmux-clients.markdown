---
layout: post
title: "How to display a message to all tmux clients"
date: 2019-01-23 18:45:04 -0600
comments: true
published: false
description: Here is how to display a message to all tmux clients
keywords: tmux
categories: 
- tmux
- tools
---

I've been using [tmux](https://github.com/tmux/tmux) a lot more lately. This resulted in me figuring out how to get [lein-test-refresh](https://github.com/jakemcc/lein-test-refresh#notifications) to send [notifications using tmux](/blog/2019/01/06/notifications-with-tmux-and-lein-test-refresh/).

The setup linked above works great for when I'm doing work all by myself. Once I started pairing with another developer it showed some problems. It only send the notifications to one of the developers. One is better than none but still not ideal.

Below is a GIF of what I'm talking about. Each window simulates a different developer pairing together on a project.

![tmux only showing one developer a notification](/images/tmux-pair-fail.gif)

This wasn't too hard to fix though. A little digging through the tmux manpage shows that `tmux display-message` takes an optional flag telling it which client gets the message. I had the idea that if I could get a list of all the clients then I could iterate over them and send a message to each.

We can get a list of clients with `tmux list-clients`. This gives output like below.

```
$ tmux list-clients
/dev/ttys002: 0 [78x41 xterm-256color] (utf8)
/dev/ttys006: 0 [78x42 xterm-256color] (utf8)
```

Cool. What we care about is the parts that look like `/dev/ttys002`. At first I used `cut` to grab those values but then I dug a bit deeper into the `tmux` manpage.

It turns out, you can specify a format to `tmux list-clients`. If you run `tmux list-clients -F "#{client_name}"` it gives only the output we care about.

```
$ tmux list-clients -F "#{client_name}"
/dev/ttys002
/dev/ttys006
```

Next, I combined that with `xargs` and to send a message to multiple clients.

![tmux xargs example](/images/tmux-xargs-example.gif)

Finally, I put it into a script called `notify` and configured `lein-test-refresh` to use that as its notification command. Script and GIF of that below. Now you and your pairs can get notifications!

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


