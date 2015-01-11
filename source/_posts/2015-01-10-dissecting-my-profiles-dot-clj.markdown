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
These profiles are available to all of your projects.

What are some useful cross-project development tools? The rest of this
post will dive into what I have in my `~/.lein/profiles.clj`. These
are all tools or settings I've found useful to have across my
development environment.

``` clojure
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

The first think you'll noticed is that I have a `:plugin-repositories
[["private-plugins" {:url "private repo url"}]]` specified. This
allows me to access [Outpace's](http://outpace.com/) internal
repository when looking for plugins. This is useful because we have
some private Leiningen project templates for quickly spinning up new
services.

The next few lines are all related. They setup
[humane-test-output](https://github.com/pjstadig/humane-test-output).
`humane-test-output` makes `clojure.test` output more readable. It
makes the output of `clojure.test` so much better. Examples can be
found in my
[Comparing Clojure Testing Libraries](/blog/2014/06/22/comparing-clojure-testing-libraries-output/)
post from last year.

``` clojure
:dependencies [[pjstadig/humane-test-output "0.6.0"]]
:injections [(require 'pjstadig.humane-test-output)
             (pjstadig.humane-test-output/activate!)]
```

Next we get to my `:plugins` section. This is the bulk of
`profiles.clj` and I imagine it is the bulk of must `profiles.clj`s.

``` clojure
:plugins [[cider/cider-nrepl "0.8.2"]
          [refactor-nrepl "0.2.2"]
          [com.jakemccrary/lein-test-refresh "0.5.5"]
          [lein-autoexpect "1.4.2"]
          [lein-ancient "0.5.5"]
          [jonase/eastwood "0.2.1"]
          [lein-kibit "0.0.8"]
          [lein-pprint "1.1.2"]]
```

The first entry is for `cider/cider-nrepl`. I edit Clojure code using
Emacs and [CIDER](https://github.com/clojure-emacs/cider) and much of
CIDER's functionality has moved to nrepl middleware found in
`cider/cider-nrepl`.

`refactor-nrepl` is next in my `:plugins`. It is required for features
in
[clj-refactor.el](https://github.com/clojure-emacs/clj-refactor.el).
I'm still pretty unfamiliar with **clj-refactor.el** and admit to
pretty much only using it for the move to let, extract to let, and
introduce let features. Even still I have `refactor-nrepl` because I'm
trying to get more familiar with clj-refactor.

`[com.jakemccrary/lein-test-refresh "0.5.5"]` is next. It enables
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh)
globally. `lein-test-refresh` runs your `clojure.test` tests whenever
a file changes in your project.

`[lein-autoexpect "1.4.2"]` follows `lein-test-refresh`. It was the
first Leiningen plugin I wrote and it enables continuous testing with
[expectations](https://github.com/jaycfields/expectations).

Both `lein-autoexpect` and `lein-test-refresh` are projects I started
and maintain. Writing `lein-autoexpect` was my first exposure to
continuous testing and it changed how I developed code. Now I find it
frustrating to develop without such a tool.

Next up is `[lein-ancient "0.5.5"]`.
[lein-ancient](https://github.com/xsc/lein-ancient) checks your
project.clj for outdated dependencies and plugins. It isn't something
that gets used every day but it is super useful when you want to
upgrade your dependencies.

I'm going to lump the next two together. The next two entries are for
`jonase/eastwood` and `lein-kibit`. They are both tools that look at
your Clojure code and report common mistakes. I find
[eastwood](https://github.com/jonase/eastwood) to be more useful than
[kibit](https://github.com/jonase/kibit) but I don't use either
regularly. Despite finding bugs with eastwood it is a tool I forget to
use.

The final plugin is `lein-pprint`.
[lein-pprint](https://github.com/technomancy/leiningen/tree/master/lein-pprint)
prints out your project map. It is useful for trying to grasp what is
going on when messing around with various Leiningen options.

The final part of my `profiles.clj` is configuration for
`lein-test-refresh.` It tells `lein-test-refresh` to use
[terminal-notifier](https://github.com/alloy/terminal-notifier) to
tell me when my tests pass or fail.

``` clojure
:test-refresh {:notify-command ["terminal-notifier" "-title" "Tests" "-message"]}
```

That is my `~/.lein/profiles.clj`. It is my own collection of Clojure
tools that I find useful to have across all of my projects.
