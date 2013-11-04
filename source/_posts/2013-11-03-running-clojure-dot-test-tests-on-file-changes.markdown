---
layout: post
title: "Running clojure.test tests on file changes"
date: 2013-11-03 20:02
comments: true
categories: [clojure, testing]
---

I recently switched [jobs](http://outpace.com) and find myself working
on a project that uses `clojure.test`. I haven't worked with
`clojure.test` since I started using
[expectations](https://github.com/jaycfields/expectations) with
[lein-autoexpect](https://github.com/jakemcc/lein-autoexpect). This
combination has spoiled me when it comes to testing Clojure code. I
can no longer stand running my tests by hand; I'm too used to having a
tool automatically run my tests. As a result I tried out some
`clojure.test` continuous test tools.

I wasn't satisfied with what I found for continuous testing with
`clojure.test`. Since I wrote `lein-autoexpect` for doing just this
task with `expectations` it was easy to fork it and  and create
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh).
`lein-test-refresh` solved the issues I was running into with the
available `clojure.test` tools.

To use `lein-test-refresh` follow these steps (latest version found in
image at end):

1. Add `[lein-test-refresh "0.1.2"]` to `:plugins` section in your
`project.clj` or `~/.lein/profiles.clj` file.
1. Run `lein test-refresh` or `lein test-refresh :growl`.
1. Enjoy your minimal feedback delays between editing your Clojure
   code and seeing if your tests pass.

Just like `lein-autoexpect`, when you pass `:growl` as a command line
argument the plugin will use growl to notify you on success or
failure. I'm fortunate enough to work on large monitors but I still
dislike taking up space with my continuous test runner. This features
enables me to only look at my test output when there is a failure.

I hope you enjoy
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh). `lein-test-refresh`
is pretty minimal but it has made using `clojure.test` much more enjoyable.

Latest version from Clojars:
![Clojars generated dependency vector](https://clojars.org/com.jakemccrary/lein-test-refresh/latest-version.svg)
