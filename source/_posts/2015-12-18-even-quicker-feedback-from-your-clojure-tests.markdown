---
layout: post
title: "Even quicker feedback from your Clojure tests"
date: 2015-12-18 14:31:14 -0600
comments: true
published: false
categories:
- clojure
- testing
---

I was recently inspired by a post on a mailing list to make the TDD
cycle with `clojure.test` and
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh) even
faster. `lein-test-refresh` is a Leiningen tool that monitors your
Clojure project's source and reloads changed namespaces before running
your tests. Using a test runner like `lein-test-refresh` gives you
some of the fastest test driven development feedback cycles possible.

Earlier this week I released version `0.12.0` of `lein-test-refresh`.
It enables even faster feedback by providing an option to only run
tests in namespaces that were just reloaded. This means you're running
the minimum number of tests after a change.

To use this new feature at
`[com.jakemccrary/lein-test-refresh 0.12.0]` to your profiles.clj or
project.clj as a plugin. An example
[project.clj](https://github.com/jakemcc/lein-test-refresh/blob/master/sample.project.clj#L3)
can be found in the project's GitHub repo.

Once you're on the latest version you can either toggle this feature
from the command line by providing a `:changes-only` flag, `lein
test-refresh :changes-only`, or by adding `:changes-only true` to your
`:test-refresh` configuration section in your project.clj or
profiles.clj.

If you turn on this feature and want to run all your tests you can
focus on your terminal and hit enter to cause all your tests to run.
Below you can see the time difference from running all my tests (first
run) and after modifying a single namespace.

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
My whole test suite isn't particularly slow, I'm pretty vigilant about
trying to keep it going fast, but even still I've been enjoying the
faster feedback.
