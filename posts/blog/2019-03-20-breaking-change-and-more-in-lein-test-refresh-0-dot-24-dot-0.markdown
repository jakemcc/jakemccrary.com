---
dated-url: true
layout: post
title: Breaking change and more in lein-test-refresh 0.24.1
date: 2019-03-20 21:30 -0500
comments: true
published: true
description: A bunch of new features arrived in version 0.24.1 of lein-test-refresh
keywords: clojure, lein-test-refresh, testing
categories:
- clojure
- testing
- lein-test-refresh
---

Today I released [lein-test-refresh](https://github.com/jakemcc/lein-test-refresh) `0.24.1`[^1]. I don't always announce new lein-test-refresh versions with an article but this release breaks some existing behavior so I thought it was worth it.

[^1]: This was originally 0.24.0 but that had a bug in it. Sorry about that.

Each of these changes is the direct result of interacting with four different `lein-test-refresh` users. Some of this took place on GitHub and others through email. Thanks to all of you for taking the time to think about improvements and notice oddities and bring them to my attention.

### Breaking change: Monitoring keystrokes to perform actions

Prior to this release, if you hit Ctrl-D then STDIN reads an EOF and `test-refresh` would quit. With version 0.24.1, `test-refresh` no longer does that. Instead, it stops monitoring for input and just keeps running tests. Since it stops monitoring for input it no longer notices when you hit Enter to cause your tests to rerun. You can still stop `lein test-refresh` by sending a SIGINT with Ctrl-C.

This change was made because there is some combination of environments where if `test-refresh` execs `/bin/bash` then it receives an EOF on STDIN. Before this change, that means `test-refresh` would quit unexpectedly. Now it will keep going.

Thanks [Alan Thompson](https://github.com/cloojure) for bringing this to my attention and taking the time to help diagnose the problem.

### You can supply your own narrowing test selector

Being able to tell `test-refresh` to narrow its focus by adding `:test-refresh/focus` as metadata on a test or namespace has quickly become a favorite feature of many users. Now you can configure a shorter keyword by specifying configuration in your profile. See the [sample project.clj](https://github.com/jakemcc/lein-test-refresh/blob/1b5165660d9e40d9394809a95b148ec758a6d56b/sample.project.clj#L61-L65) for how to set this up.

Thanks [Yuri Govorushchenko](https://github.com/metametadata) for the suggestion.

### Experimental: Run in a repl

I've turned down this feature in the past but a narrower request came up and I thought it seemed useful. `test-refresh` now exposes a function you can call in a repl to run `test-refresh` in that repl. This makes the repl useless for any other task. To do this, first add `lein-test-refresh` as a dependency instead of a plugin to your project.clj. Then, require the namespace and call the function passing in one or more paths to your test directories. Example below.

```clojure
user=> (require 'com.jakemccrary.test-refresh)
nil
user=> (com.jakemccrary.test-refresh/run-in-repl "test")
*********************************************
*************** Running tests ***************
```

[This request](https://github.com/jakemcc/lein-test-refresh/issues/80) was done so that you can run it in Cursive's repl and gain the ability to click on stacktraces. Thanks [Klaus Wuestefeld](https://github.com/klauswuestefeld) for bringing this up again with a really solid and focused use case.

### Better output on exceptions while reloading

This was a [pull request](https://github.com/jakemcc/lein-test-refresh/pull/81) from [Minh Tuan Nguyen](https://github.com/minhtuannguyen). Now figuring out where to look for the error will be a little easier.


## Thank you

Thanks to all the users of lein-test-refresh. I've found it to be very valuable to the way I work and I'm very happy that others do as well.
