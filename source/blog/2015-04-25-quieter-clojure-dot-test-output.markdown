---
dated-url: true
layout: post
title: Quieter clojure.test output
date: 2015-04-25 15:40
comments: true
published: true
categories:
- clojure
- testing
---

If you use `clojure.test` then there is a good chance you've been annoyed by all the [output](https://github.com/jakemcc/lein-test-refresh/issues/33) when you run your tests in the terminal.
When there is a test failure you have to scroll through pages of output to find the error.

With release `0.9.0` of [lein-test-refresh](https://github.com/jakemcc/lein-test-refresh) you can minimize the output of `clojure.test` and **only** see failure and summary messages.
To enable this feature add `:quiet true` to the `:test-refresh` configuration map in either your project.clj or profiles.clj file.
If you configure `lein-test-refresh` in `~/.lein/profiles.clj` then turning on this feature looks like the following.[^1]

[^1]: More configuration options can be found [here](https://github.com/jakemcc/lein-test-refresh/blob/master/sample.project.clj#L5-L24)

``` clojure
{:user {:plugins [[com.jakemccrary/lein-test-refresh "0.9.0"]]
        :test-refresh {:quiet true}}}
```

Setting up your profiles.clj like above allows you to move to Clojure project in your terminal, run `lein test-refresh`, and have your `clojure.test`s run whenever a file changes.
In addition, your terminal won't show the usual _Testing a.namespace_ output.

Below is what you typically see when running `clojure.test` tests in a terminal.
I had to cut most of the _Testing a.namespace_ messages from the picture.

![Normal view of test output](/images/not-quiet-test-output.png)

The following picture is with quiet mode turned on in `lein-test-refresh`.
No more _Testing a.namespace_ messages!
No more scrolling through all your namespaces to find the failure!

![Minimal output in console](/images/minimal-test-output.png)

I just released this feature so i haven't had a chance to use it too much.
I imagine it may evolve to change the output more.
