---
layout: post
title: Notify yourself when a task finishes
date: 2025-11-30 15:33 -0600
comments: true
published: false
description: Desktop notifications when tasks finish are great.
categories:
- osx
- tools
- clojure
- terminal-notifier
---

I often have some task running in a hidden terminal that performs some action when files change.
This is most often running Clojure tests whenever a file changes through either [test-refresh](https://github.com/jakemcc/lein-test-refresh) or [lein-autoexpect](https://github.com/jakemcc/lein-autoexpect).
Another common watch task is rendering this website whenever one of the markdown files changes.

I don't like needing to have these processes always visible, since I mostly only care about when they finish.
When running unit tests, I don't need to see the output unless a test is failing.
When writing articles, I only care about when the rendering is done so I know I can refresh my browser to review the output.

On macOS, one way of doing this is using [terminal-notifier](https://github.com/julienXX/terminal-notifier).
`terminal-notifier` makes it trivial to send notifications.

Below is the script I run while working on this website.
It uses `entr` to monitor the input files; when changes are detected, it renders this site using my homegrown [Babashka](https://babashka.org/) static site generator, and when that finishes, it uses `terminal-notifier` to alert me.

```bash
#!/bin/bash
while sleep 0.5; do 
    rg bb templates source --files -t css -t clojure -t markdown -t html \
        | entr -d -s 'rm -rf output/*; time /opt/homebrew/bin/bb render --preview true && terminal-notifier -message "Rendering complete"';
done
```

This site renders quickly, so I can usually make some edits, save, and toggle to a browser to refresh and see the output. 
Still, it is nice to see that little notification pop-up appear on my screen so I know for sure that if I hit refresh, I'm seeing the latest render.

When I'm running my Clojure tests, both `lein-autoexpect` and `test-refresh` send a notification with a pass or fail message based on the status of the unit tests that just ran.

I'd encourage you to think about what processes you might want to get notifications from when they are done and look into how to set that up.
`terminal-notifier` works great on macOS.
I can't make recommendations for other operating systems since it has been years since I've used any alternatives besides SSHing into a Linux server.

It is worth the effort to figure out how to have notifications.
They remove a trivial inconvenience (having to switch programs, needing to keep a window visible on your screen) and make life a little better.
By stacking small, slightly life-improving techniques, all of a sudden you find yourself much more productive.
