---
layout: post
title: "Continuous testing with Clojure and expectations"
date: 2011-12-15 22:30
comments: true
categories: [clojure, code, testing]
---

I've recently started using [Jay Fields'](http://jayfields.com/) Clojure testing library, [`expectations`](https://github.com/jaycfields/expectations). I'm not going to explain `expectations`, Jay already did a great job on his [blog](http://blog.jayfields.com/2011/11/clojure-expectations-introduction.html), but I will quote its [Github](https://github.com/jaycfields/expectations) page.

>expectations is a minimalist's testing framework

The above quote is absolutely true, which is one of the major reasons I'm liking `expectations`. It hasn't been all sunshine though, when I first started using it I had a major problem. It slowed down my usual Clojure workflow.

Up until this point I had stuck to using `clojure.test`. Combined with emacs, slime, swank, and `clojure-test-mode` I found the time between making a change to code and running tests to be minimal.

When I switched to `expectations` the time it took between making a code change and running tests increased. With `expectations` I couldn't reevaluate my buffer to get the new tests in my repl environment. Doing so caused the new tests to be there along with the old tests. This meant I needed to switch to the command line to run my tests. This caused me to incur the startup costs of the jvm simply to run my expectations (tests). This was a huge cost compared to what I was used to before.

## Introducing `lein-autoexpect`

To fix my problem I wrote [`lein-autoexpect`](https://github.com/jakemcc/lein-autoexpect). `lein-autoexpect` is a [`Leiningen`](https://github.com/technomancy/leiningen/) plugin that monitors a project's source and test directory and when a Clojure file changes it reloads the affected namespaces and runs all the expectations. Using this plugin my turn around time from modifying code to running all of my expectations is practically nothing. Without the cost of the jvm startup there is practically no time wasted between when code is saved and tests are run.

To use `lein-autoexpect` simply add `[lein-autoexpect "0.0.2"]` to your `project.clj` file and fetch the dependency. Then at the command line run `lein autoexpect`. You'll see your tests run and then it will just hang there, eagerly waiting for code to change.

``` bash
    $ lein autoexpect
    *********************************************
    *************** Running tests ***************
    Ran 3 tests containing 3 assertions in 16 msecs
    0 failures, 0 errors.
```

Next time you end up saving you'll see your tests run again and the following example output appears.

``` bash
    *********************************************
    *************** Running tests ***************
    Ran 4 tests containing 4 assertions in 3 msecs
    0 failures, 0 errors.
```

`lein-autoexpect` tries to clearly delimit each test session with the banner made of `*`. This helps keep different runs separate when scrolling through your terminal.

This style of testing is called [continuous testing](http://blog.objectmentor.com/articles/2007/09/20/continuous-testing-explained). If you haven't tried it, I would highly recommend giving it a shot. Even just using it for the last few days changed how I think testing should be done.

