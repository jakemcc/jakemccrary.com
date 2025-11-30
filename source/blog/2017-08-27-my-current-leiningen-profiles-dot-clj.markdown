---
dated-url: true
layout: post
title: My current Leiningen profiles.clj
date: 2017-08-27 19:06 -0500
comments: true
published: true
description: Here is a snapshot of my current Leiningen profiles.clj
keywords: clojure, leiningen, profiles.clj
categories:
- leiningen
- clojure
---

Nearly three years ago I wrote an overview of my [Leiningen profiles.clj](/blog/2015/01/11/overview-of-my-leiningen-profiles-dot-clj/).
That post is one of my most visited articles, so I thought I'd give an update on what I currently keep in `~/.lein/profiles.clj`.

``` clojure profiles.clj
{:user {:plugin-repositories [["private-plugins" {:url "private url"}]]
        :dependencies [[pjstadig/humane-test-output "0.8.2"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        :plugins [[io.sattvik/lein-ancient "0.6.11"]
                  [lein-pprint "1.1.2"]
                  [com.jakemccrary/lein-test-refresh "0.21.1"]
                  [lein-autoexpect "1.9.0"]]
        :signing {:gpg-key "B38C2F8C"}
        :test-refresh {:notify-command ["terminal-notifier" "-title" "Tests" "-message"]
                       :quiet true
                       :changes-only true}}}
```

The biggest difference between my `profiles.clj` from early 2015 and now is that I've removed all of the CIDER related plugins.
I still use CIDER, but CIDER no longer requires you to list its dependencies explicitly.

I’ve also removed Eastwood and Kibit from my toolchain.
I love static analysis, but these tools fail too frequently with my projects.
As a result, I rarely used them and I’ve removed them.
Instead, I’ve started using [joker](https://github.com/candid82/joker) for some basic static analysis and am really enjoying it.
It is fast, and it has made refactoring in Emacs noticeably better.

[lein-test-refresh](https://github.com/jakemcc/test-refresh), [lein-autoexpect](https://github.com/clojure-expectations/lein-autoexpect), and [humane-test-output](https://github.com/pjstadig/humane-test-output) have stuck around and have been updated to the latest versions.
These tools make testing Clojure much nicer.

I'm also taking advantage of some new features that [lein-test-refresh](https://github.com/jakemcc/test-refresh) provides.
These settings enable the most reliable, fastest feedback possible while writing tests.
My [recommended testing setup](/blog/2016/06/20/my-recommended-clojure-testing-setup/) article goes into more details.

`lein-ancient` and `lein-pprint` have stuck around.
I rarely use `lein-pprint` but it comes in handy when debugging project.clj problems.
`lein-ancient` is great for helping you keep your project's dependencies up to date.
I use a forked version that contains some changes I need to work with my company's private repository.

And there you have it.
My updated profiles.clj[^1].

[^1]: Some of you might wonder why I don't just link to this file in version control somewhere? Well, it is kept encrypted in a git repository because it also contains some secrets that should not be public that I've removed for this post.