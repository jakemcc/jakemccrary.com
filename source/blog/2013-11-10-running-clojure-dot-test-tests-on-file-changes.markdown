---
dated-url: true
layout: post
title: "Running clojure.test tests on file changes"
date: 2013-11-11 20:02
comments: true
categories: [clojure, testing]
---

I recently switched [companies](http://outpace.com) and find myself working
on a project that uses `clojure.test`. I haven't worked with
`clojure.test` since I started using
[expectations](https://github.com/jaycfields/expectations) with
[lein-autoexpect](https://github.com/jakemcc/lein-autoexpect). This
combination spoiled me when it comes to testing Clojure code. I
can no longer stand running tests by hand; I'm too used to having a
tool run them for me. As a result I tried out some
`clojure.test` continuous testing tools.

I wasn't satisfied with what I found. Since I wrote `lein-autoexpect`,
a continous tester for `expectations`, it was easy for me to fork it
and and create
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh).
`lein-test-refresh` solves the issues I ran into with the other
`clojure.test` tools.

To use `lein-test-refresh` follow these steps (latest version found in
image at end):

1. Add `[com.jakemccrary/lein-test-refresh "0.1.2"]` to the `:plugins`
section in your `project.clj` or `~/.lein/profiles.clj` file.
1. Run `lein test-refresh` or `lein test-refresh :growl`.
1. Enjoy your minimal feedback delays between editing your Clojure
   code and seeing if your tests pass.

[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh)
watches the source and test directories specified in your
`project.clj` and reloads code when files changes. After reloading
your code your `clojure.test` tests are run and the output is printed
to your console. When you pass `:growl` as a command line argument the
plugin will use growl to notify you of success and failures. This is
one of my favorite features about `lein-test-refresh` as it allows me
to continuously run my tests without taking up space on my monitors.

I hope you enjoy
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh). It
has made using `clojure.test` much more enjoyable.

Latest version from Clojars:
![Clojars generated dependency vector](https://clojars.org/com.jakemccrary/lein-test-refresh/latest-version.svg)