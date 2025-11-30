---
dated-url: true
layout: post
title: Built-in test narrowing with lein-test-refresh
date: 2018-07-16 14:47 -0500
comments: true
published: true
description: lein-test-refresh now contains a built-in test selector. This lets you
  quickly focus on a subset of your tests without restarting your testing process.
keywords: clojure, testing, lein-test-refresh
categories:
- clojure
- testing
- lein-test-refresh
---

If you follow my work you probably know that I value fast feedback cycles.
Most of the open-source I maintain was developed to enable faster feedback cycles.
This is why [lein-test-refresh](https://github.com/jakemcc/test-refresh/) and [lein-autoexpect](https://github.com/clojure-expectations/lein-autoexpect) were originally created.


Leiningen supports [test selectors](https://github.com/technomancy/leiningen/blob/master/doc/TUTORIAL.md#tests) and lein-test-refresh [does as well](https://github.com/jakemcc/test-refresh/blob/master/CHANGES.md#040).
This lets you start-up a testing session and only run tests or namespaces with certain metadata on them.
This is a super useful feature as it lets you narrow your testing scope to one (or a handful) of tests while working on solving a specific problem.

lein-test-refresh now has built-in functionality that allows you to focus your test scope without restarting the Leiningen test process.
If lein-test-refresh sees a `deftest` or `ns` form marked with `:test-refresh/focus true` in its metadata, then it will only run tests marked with `:test-refresh/focus`.

Below is an example of what this looks like.

```clojure
(deftest ^:test-refresh/focus test-addition
  (is (= 2 (+ 1 1))))
```

This functionality has only been available for a short period of time and I've already found it useful.
I think you will too.
Enjoy.