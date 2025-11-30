---
dated-url: true
layout: post
title: Even quicker feedback from your Clojure tests
date: 2015-12-18 15:29 -0600
comments: true
published: true
description: Get even quicker feedback with lein-test-refresh's newest feature.
categories:
- clojure
- testing
---

I was recently inspired by a post on a mailing list to make the TDD cycle with `clojure.test` and [lein-test-refresh](https://github.com/jakemcc/test-refresh) even faster.
`lein-test-refresh` is a Leiningen tool that monitors your Clojure project's source, reloads changes, and then runs your tests.
Tools like it provide some of the fastest feedback cycles possible.

To make the feedback cycle even faster I added the option to only run tests in changed namespaces.
This means you're running the minimum number of tests after a change.
Version 0.12.0 of `lein-test-refresh` was released earlier this week with this feature.

To use it add `[com.jakemccrary/lein-test-refresh 0.12.0]` as a plugin to your profiles.clj or project.clj.
An example [project.clj](https://github.com/jakemcc/test-refresh/blob/master/sample.project.clj#L3) can be found in the project's GitHub repo.

Once you're on the latest version you can toggle this feature from the command line by providing a `:changes-only` flag, `lein test-refresh :changes-only`, or by adding `:changes-only true` to your `:test-refresh` configuration section in your project.clj or profiles.clj.
When the feature is on you can still run all your tests by hitting `enter` in the terminal running `lein test-refresh`.

Below is an example of the time difference between running all my tests and the tests in a single namespace.

```
Ran 49 tests containing 219 assertions.
0 failures, 0 errors.

Passed all tests
Finished at 14:42:41.655 (run time: 2.006s)
*********************************************
*************** Running tests ***************
:reloading (lumanu.utils-test)

Ran 1 tests containing 3 assertions.
0 failures, 0 errors.

Passed all tests
Finished at 14:43:12.648 (run time: 0.085s)
```

I've been using this feature for about a week now and am enjoying it.
My whole test suite isn't particularly slow but even still I've been enjoying the faster feedback.