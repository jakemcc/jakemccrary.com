---
dated-url: true
layout: post
title: My recommended Clojure testing setup
date: 2016-06-20 21:03 -0500
comments: true
published: true
description: I've been test driving Clojure for over five years and this is my recommended
  setup.
keywords: clojure, testing, TDD, lein-test-refresh, test driven development
categories:
- clojure
- testing
---

Occasionally, either on Stack Overflow or in the [Clojurians](http://clojurians.net/) Slack group, someone will ask what tools they should use to test Clojure code.
Below is what I would currently recommend.
I've come to this recommendation through observing teams using a variety of testing tools and through my own use them.

> Use clojure.test with
> [humane-test-output](https://github.com/pjstadig/humane-test-output)
> and [lein-test-refresh](https://github.com/jakemcc/lein-test-refresh).

### Use clojure.test

clojure.test is ubiquitous and not a big departure from other languages' testing libraries.
It has its warts but your team will be able to understand it quickly and will be able to write maintainable tests.

### Use humane-test-output

You should use clojure.test with [humane-test-output](https://github.com/pjstadig/humane-test-output).
Together they provide a testing library that has minimal additional syntax and good test failure reporting.

### Use lein-test-refresh

If you're not using a tool that reloads and reruns your tests on file changes then you are wasting your time.
The delay between changing code and seeing test results is drastically reduced by using a tool like [lein-test-refresh](https://github.com/jakemcc/lein-test-refresh).
Nearly everyone I know who tries adding lein-test-refresh to their testing toolbox continues to use it.
Many of these converts were not newcomers to Clojure either, they had years of experience and had already developed workflows that worked for them.

### Use lein-test-refresh's advanced features

[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh) makes development better even if you don't change any of its settings.
It gets even better if you use some of its advanced features.

Below is a stripped down version of my `~/.lein/profiles.clj`.
The `:test-refresh` key points towards my recommended lein-test-refresh settings.

```clojure
{:user {:dependencies [[pjstadig/humane-test-output "0.8.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        :plugins [[com.jakemccrary/lein-test-refresh "0.16.0"]]
        :test-refresh {:notify-command ["terminal-notifier" "-title" "Tests" "-message"]
                       :quiet true
                       :changes-only true}}}
```

These settings turn on notifications when my tests finish running (`:notify-command` setting), make clojure.test's output less verbose (`:quiet true`), and only run tests in namespaces affected by the previous code change (`:changes-only true`).
These three settings give me the quickest feedback possible and free me from having the terminal running `lein test-refresh` visible.

Quick feedback lets you make changes faster.
If you're going to write tests, and you should write tests, having them run quickly is powerful.
After years of writing Clojure, this is my current go-to for testing Clojure code and getting extremely fast feedback.