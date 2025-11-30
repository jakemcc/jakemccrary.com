---
dated-url: true
layout: post
title: "Releasing lein-test-refresh 0.3.0"
date: 2014-01-12 20:12
comments: true
categories: [clojure, testing]
---

At the suggestion of my coworker [Jeff Bay](http://www.xpteam.com/) you can now hit a single keystroke to cause [lein-test-refresh](https://github.com/jakemcc/test-refresh) to rerun your tests.
Now you can hit enter in the terminal running `lein test-refresh` to cause your tests to be run.

Add the below text to your project.clj to start using `lein-test-refresh` today.
![Clojars generated dependency vector](https://clojars.org/com.jakemccrary/lein-test-refresh/latest-version.svg)

As a reminder if you call you can pass the argument `:growl` to lein-test-refresh.
If you pass `:growl` as an argument then you'll be notified of test success and failures through [growl](http://growl.info/).
On top of the quick feedback cycles that [lein-test-refresh](https://github.com/jakemcc/test-refresh) (and [lein-autoexpect](https://github.com/jakemcc/lein-autoexpect)) provides the growl notification is my favorite feature.
I'd highly recommend giving it a shot.