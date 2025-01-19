---
dated-url: true
layout: post
title: "Flexible notification of Clojure tests failing"
date: 2014-02-02 18:11
comments: true
categories: [clojure, testing]
---

[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh) has
always supported notifying you of your tests' status through growl.
With the release of version 0.3.4 it now will notify you using
whatever program you want.

To make my Mac whisper my the results of running my tests I can use
the following project.clj

``` clojure
    (defproject sample "1.2.3"
      :dependencies [[org.clojure/clojure "1.5.1"]]
      :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.3.4"]]}}
      :test-refresh {:notify-command ["say" "-v" "Whisper"]})
```

The specification of the command is found in the `:test-refresh
{:notify-command ["say" "-v" "Whisper"]}` entry in the above
project.clj. After running your tests
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh) will
pass a (usually) short summary message as the final parameter to the
specified command.

Now you can finally have the results of running
your tests whispered to you.
