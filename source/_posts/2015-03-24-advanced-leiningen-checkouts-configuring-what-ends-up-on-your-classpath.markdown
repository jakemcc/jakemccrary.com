---
layout: post
title: "Advanced Leiningen checkouts: configuring what ends up on your classpath"
date: 2015-03-24 20:06:12 -0500
comments: true
published: false
categories:
- leiningen
- clojure
---

[Leiningen](http://leiningen.org/) checkout dependencies are a useful
feature. Checkout dependencies allow you to work on a library and consuming
project at the same time. By setting up checkout dependencies you can
skip running `lein install` in the library project; it appears on the
classpath of the consuming project. An example of what this looks like
can be found in the
[Leiningen documentation](https://github.com/technomancy/leiningen/blob/master/doc/TUTORIAL.md#checkout-dependencies)
or in a
[previous post](http://jakemccrary.com/blog/2012/03/28/working-on-multiple-clojure-projects-at-once/)
of mine.

By default, Leiningen adds the `:source-paths`, `:test-paths`,
`:resource-paths`, and `:compile-path` directories of the checkout
projects to your consuming project's classpath. It also recurses and
adds the checkouts of your checkouts (and keeps recursing).

You can override what gets added to your classpath by
`:checkout-deps-shares` to your project.clj. This key's value should
be a vector of functions that when applied to your checkouts' project
map return the paths that should be included on the classpath. The
default values can be found
[here](https://github.com/technomancy/leiningen/blob/ff84da697249184874b528950048981621ac0b61/leiningen-core/src/leiningen/core/project.clj#L488-L492)
and an example of overriding the default behavior can be found in the
[sample.project.clj](https://github.com/technomancy/leiningen/blob/ff84da697249184874b528950048981621ac0b61/sample.project.clj#L320-L321).

I ran into a situation this week where having my checkouts'
`:test-paths` on the classpath caused issues my consuming project.
Some namespace refreshing code caused the namespaces found in the
`:test-paths` to reload which caused some issues. The first pass at
fixing this problem was to add `:checkout-deps-shares
[:source-paths :resource-paths :compile-path]` to my project.clj. This
**didn't** work. My project.clj looked something like below.

``` clojure
(defproject example "1.2.3-SNAPSHOT"
  :dependencies [[library "1.2.2"]
                 [org.clojure/clojure "1.6.0"]]
  :checkout-deps-shares [:source-paths :resource-paths :compile-path])
```

Why didn't it work? Well it didn't work because of how Leiningen
handles combining matching keys. When Leiningen merges the various
configuration maps (from merging profiles, merging defaults, etc) and
it encounters values that are collections it combines them (more
details found in
[documentation](https://github.com/technomancy/leiningen/blob/master/doc/PROFILES.md#merging)).
Using `lein pprint :checkout-deps-shares` will show what it looks
like. We've ended up with both the defaults and our specified values.

``` console
$ lein pprint :checkout-deps-shares
(:source-paths
 :resource-paths
 :compile-path
 :source-paths
 :test-paths
 :resource-paths
 :compile-path
 #<Var@43e3a075:
   #<classpath$checkout_deps_paths leiningen.core.classpath$checkout_deps_paths@6761b44b>>)
```

This is a pretty easy change. To tell Leiningen to replace the value
you add the `^:replace` metadata to the value. Below is the same
project.clj as above _except_ it uses `^:replace`.

``` clojure
(defproject example "1.2.3-SNAPSHOT"
  :dependencies [[library "1.2.2"]
                 [org.clojure/clojure "1.6.0"]]
  :checkout-deps-shares ^:replace [:source-paths :resource-paths :compile-path])
```

This solves the problem of `:test-paths` showing up on the classpath.
It introduces another problem though. Checkouts' checkout dependencies
no longer show up on the classpath. This is because
`leiningen.core.classpath/checkout-deps-paths` is no longer applied to
the checkouts.

It introduced another problem though. The value of
`:checkout-deps-shares` above stops Leiningen from recursing and
picking up my checkouts' checkout dependencies. The vector I set
dropped `#'classpath/checkout-deps-paths`. It took a while to figure
out how to add that to my project.clj.

This next snippet fails. It runs but fails to add the checkouts'
checkout dependencies to the classpath.

``` clojure
:checkout-deps-shares ^:replace [:source-paths :resource-paths :compile-path
                                 leiningen.core.classpath/checkout-deps-paths]
```

The next attempt failed quicker. It causes an exception to be thrown.
At least it fails fast.

``` clojure
:checkout-deps-shares ^:replace [:source-paths :resource-paths :compile-path
                                 #'leiningen.core.classpath/checkout-deps-paths]
```

The next one works. It takes advantage of dynamic eval through
[read-eval](https://github.com/technomancy/leiningen/blob/master/doc/PROFILES.md#dynamic-eval)
syntax.

``` clojure
:checkout-deps-shares ^:replace [:source-paths :resource-paths :compile-path
                                 #=(eval leiningen.core.classpath/checkout-deps-paths)]
```

Hopefully seeing this is useful and, if you ever find yourself in this
situation, saves you from having to figure it out. The full example
project.clj is below.

``` clojure
(defproject example "1.2.3-SNAPSHOT"
  :dependencies [[library "1.2.2"]
                 [org.clojure/clojure "1.6.0"]]
  :checkout-deps-shares ^:replace [:source-paths :resource-paths :compile-path
                                   #=(eval leiningen.core.classpath/checkout-deps-paths)])
```
