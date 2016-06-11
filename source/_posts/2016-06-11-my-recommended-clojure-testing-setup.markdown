---
layout: post
title: "My recommended Clojure testing setup"
date: 2016-06-11 13:42:11 -0500
comments: true
published: false
description: I've been test driving Clojure for over five years and this is my recommended setup.
keywords: 'clojure, testing, TDD, lein-test-refresh, test driven development'
categories: 
- clojure
- testing
---

Frequently I read a question on Stack Overflow or in the
[Clojurians](http://clojurians.net/) Slack team about recommendations
for testing in Clojure. For my entire development career I've worked
primarily with a test driven development mindset and have some
recommendations when it comes to testing in Clojure.

### 1. Use clojure.test with humane-test-output

You should use clojure.test with
[humane-test-output](https://github.com/pjstadig/humane-test-output)
because together they provide a testing library that has minimal
additional syntax, great output for failing tests (thanks
humane-test-output), and clojure.test is available anywhere Clojure
is.

### 2. Use a tool to rerun your tests on file change

If you're not using a tool that reloads namespaces on file changes and
then reruns your test you are wasting your time. Nearly everyone I
know who tries a tool like this sticks with it and reports changes to
their development style. With a proper tool you'll cut your feedback
cycle drastically.

I'm a bit biased since I'm the author, but I'd recommend
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh) as
that tool to refresh and rerun your tests.

### 3. Use lein-test-refresh's advanced features

lein-test-refresh makes development better even if you don't change
any of its settings. It gets even better if you use some of its
advanced features.

Below is a stripped down version of my `~/.lein/profiles.clj`. The
settings I put in my `~/.lein/profiles.clj` are below. The
`:test-refresh` key points towards my recommended lein-test-refresh settings.

```clojure
{:user {:dependencies [[pjstadig/humane-test-output "0.8.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        :plugins [[[com.jakemccrary/lein-test-refresh "0.15.0"]]]
        :test-refresh {:notify-command ["terminal-notifier" "-title" "Tests" "-message"]
                       :quiet true
                       :changes-only true}}}
```

Those settings turn on notifications when my tests finish running
(`:notify-command` setting), make clojure.test's output less verbose
(`:quiet true`), and only runs tests in namespaces affected by the
previous change (`:changes-only true`). These three settings give me
the quickest feedback and free me from always having my terminal
running `lein test-refresh` visible.

Quick feedback from tests lets you make changes faster. If you're
going to write tests, which I very much think you should, you should
be concerned with getting feedback quickly. This setup is my fastest
way of getting feedback.
