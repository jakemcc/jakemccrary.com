---
layout: post
title: "Dissecting my profiles.clj"
date: 2015-01-10 15:59:38 -0600
comments: true
published: false
categories: 
- leiningen
- clojure
---

Leiningen has the concept of
[profiles](https://github.com/technomancy/leiningen/blob/master/doc/PROFILES.md).
One thing profiles are useful for is allowing you to have development
tools available to a project without having them as dependencies when
you release your project. An example of when you might want to do this
is when you are using a testing library like
[expectations](https://github.com/jaycfields/expectations).

Some development tools, such as
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh), are
useful to have across most of your Clojure projects. Rather nicely,
Leiningen supports adding global profiles to `~/.lein/profiles.clj`.
These profiles are available in all your projects.

Below is most of my `profiles.clj`. I've removed some sensitive
settings and what is left are the development tools that I find
useful.

``` clojure Entire :user profile
{:user {:plugin-repositories [["private-plugins" {:url "private repo url"}]]
        :dependencies [[pjstadig/humane-test-output "0.6.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        :plugins [[cider/cider-nrepl "0.8.2"]
                  [refactor-nrepl "0.2.2"]
                  [com.jakemccrary/lein-test-refresh "0.5.5"]
                  [lein-autoexpect "1.4.2"]
                  [lein-ancient "0.5.5"]
                  [jonase/eastwood "0.2.1"]
                  [lein-kibit "0.0.8"]
                  [lein-pprint "1.1.2"]]
        :test-refresh {:notify-command ["terminal-notifier" "-title" "Tests" "-message"]}}}
```

`:plugin-repositories [["private-plugins" {:url "private repo url"}]]`
sets a private plugin repository. This allows me to use
[Outpace's](http://outpace.com/) private Leiningen templates for
setting up new projects for work.

The next few lines are all related. They setup
[humane-test-output](https://github.com/pjstadig/humane-test-output).
`humane-test-output` makes `clojure.test` output more readable. It
makes using `clojure.test` much more enjoyable. I highly recommend it.
Sample output can be found in my
[Comparing Clojure Testing Libraries](/blog/2014/06/22/comparing-clojure-testing-libraries-output/)
post.

``` clojure humane-test-output setup in the :user profile
:dependencies [[pjstadig/humane-test-output "0.6.0"]]
:injections [(require 'pjstadig.humane-test-output)
             (pjstadig.humane-test-output/activate!)]
```

Next we get to my `:plugins` section. This is the bulk of
my `profiles.clj`.

``` clojure :plugins section of my :user profile
:plugins [[cider/cider-nrepl "0.8.2"]
          [refactor-nrepl "0.2.2"]
          [com.jakemccrary/lein-test-refresh "0.5.5"]
          [lein-autoexpect "1.4.2"]
          [lein-ancient "0.5.5"]
          [jonase/eastwood "0.2.1"]
          [lein-kibit "0.0.8"]
          [lein-pprint "1.1.2"]]
```

The first entry is for `cider/cider-nrepl`. I write Clojure using
Emacs and [CIDER](https://github.com/clojure-emacs/cider) and much of
CIDER's functionality exists in nrepl middleware found in
`cider/cider-nrepl`. This dependency is required for me to be
effective while writing Clojure.

`refactor-nrepl` is next.
[clj-refactor.el](https://github.com/clojure-emacs/clj-refactor.el)
requires it for some refactorings. I actually don't use any of those
refactorings (I only use move to let, extract to let, and
introduce let refactorings) but I still keep it around.

`com.jakemccrary/lein-test-refresh` is next. This lets me use
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh)
globally. `lein-test-refresh` runs your `clojure.test` tests whenever
a file changes in your project. This is another key development tool
in my process.

Up next is `lein-autoexpect`. It was the first Leiningen plugin I
wrote and it enables continuous testing with
[expectations](https://github.com/jaycfields/expectations).

Both `lein-autoexpect` and `lein-test-refresh` are projects I created
and maintain. Writing `lein-autoexpect` was my first
exposure to continuous testing and it changed how I develop code. I
find it frustrating to develop without such a tool.

Next up is [lein-ancient](https://github.com/xsc/lein-ancient). It
checks your project.clj for outdated dependencies and plugins. It
isn't something that gets used every day but it is super useful when
you need it.

The next two entries are for
[jonase/eastwood](https://github.com/jonase/eastwood) and
[lein-kibit](https://github.com/jonase/kibit). They are both tools
that look at your Clojure code and report common mistakes. I don't use
either consistently but I do find them useful. I've found bugs with eastwood.

The final plugin is `lein-pprint`.
[lein-pprint](https://github.com/technomancy/leiningen/tree/master/lein-pprint)
prints out your project map. It is useful for trying to grasp what is
going on when messing around with various Leiningen options.

The final part, seen below, of my `profiles.clj` is configuration for
`lein-test-refresh.` It configures `lein-test-refresh` to use
[terminal-notifier](https://github.com/alloy/terminal-notifier) to
notify me when my tests pass or fail. Using a continuous tester that
allows flexible notification is useful. Not having to glance at a
terminal to see if your tests are passing or failing is great.

``` clojure
:test-refresh {:notify-command ["terminal-notifier" "-title" "Tests" "-message"]}
```

That is my `~/.lein/profiles.clj`. I don't think it contains anything
mind blowing but it definitely contains a useful collection of Clojure
development tools. I encourage you to check out them out and to think
about what tools you should be putting into your global `:user`
profile.
