---
layout: post
title: Notify yourself when a task finishes
date: 2025-11-30 15:33 -0600
comments: true
published: false
description: TODO
categories:
- TODO
---

I often have some task running in a hidden terminal that performs some action with files change.
This is most often running Clojure tests whenever a file changes through either test-refresh (TODO: link test-refresh github repo) or lein-autoexpect (TODO: link lein-autoexpect).
Another common watch task is rendering this website whenever one of the markdown files changes.

I don't like needing to have these processes always visible, since I mostly only care about when they finish.
With running unit tests, I don't need to see the output unless a test is failing.
When writing articles, I only care about when the rendering is done so I know I can refresh my browser to review the output.

On MacOS, one way of doing this is using [terminal-notifier](https://github.com/julienXX/terminal-notifier).
`terminal-notifier` makes it trivial to send notifications.

Below is the script I run while working on this website.
It uses `entr` for monitoring for changes to the input files and when changes are detected, it renders this site using my homegrown Babashka code and when that finishes, it uses `terminal-notifier` to alert me.

```bash
#!/bin/bash
while sleep 0.5; do 
    rg bb templates source --files -t css -t clojure -t markdown -t html \
        | entr -d -s 'rm -rf output/*; time /opt/homebrew/bin/bb render --preview true && terminal-notifier -message "Rendering complete"';
done
```

This site renders fast so I can usually make some edits, save, and toggle to a browser to refresh and see the output. 
Still, it is nice to see that little notification pop-up appear on my screen so I know for sure if I hit refresh I'm seeing the latest render.

When I'm running my Clojure tests, both `lein-autoexpect` and `test-refresh` send a notification with a Pass or Failed message based on the status of the just ran unit tests.

I'd encourage you to think about what processes you might want to get a notification from when they are done and look into how to set that up.
`terminal-notifier` works great on MacOS.
I can't make recommendations for other operating systems since it has been years since I've used any alternatives besides sshing into a Linux server.
It is worth the effort to figure out how to have notifications.
They remove a trivial inconvenience (having to switch programs, needing to keep a window visible on your screen) and make life a little better.
By stacking small, slightly life improving techniques, all of a sudden you find yourself much more productive.

