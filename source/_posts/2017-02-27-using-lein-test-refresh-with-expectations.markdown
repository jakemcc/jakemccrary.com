---
layout: post
title: Using lein-test-refresh with expectations
date: 2017-02-27 09:19 -0600
comments: true
published: true
description: Quick introduction to using lein-test-refresh with expectations.
keywords: clojure, clojure.test, expectations, lein-autoexpect, lein-test-refresh,
  autoexpect, test-refresh
categories:
- testing
- clojure
- lein-test-refresh
---

The 2.2.0 release[^1]
of
[expectations](https://github.com/clojure-expectations/expectations/blob/master/CHANGELOG.md#changes-in-version-220) adds
a
`clojure.test`
[compatible syntax](https://clojure-expectations.github.io/clojure-test.html). The release
adds the `defexpect` macro which forces you to name your test but then
generates code that is compatible with `clojure.test`.

Why would you want this? Because `clojure.test` is the built-in
testing library for Clojure, an entire ecosystem has been built around
it. Tool support for `clojure.test` is always going to be ahead of
support for the original `expectations`. By using the new
`clojure.test` compatible syntax, `expectations` can take
advantage of all the tools built for `clojure.test`.

### Using lein-test-refresh with expectations

If you move to the new `clojure.test` compatible syntax, you can start
using
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh) to
automatically rerun your tests when your code
changes. `lein-test-refresh` is a fork of the original expectations autorunner, [lein-autoexpect](https://github.com/clojure-expectations/lein-autoexpect), but it has grown to have more features than its original inspiration. Now you can use it with `expectations`[^2].

Below is a sample `project.clj` that uses `lein-test-refresh` with the latest expectations.

```clojure
(defproject expectations-project "0.1.0-SNAPSHOT"
  :description "Sample project using expectations"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :plugins [[com.jakemccrary/lein-test-refresh  "0.18.1"]]
  :profiles {:dev {:dependencies [[expectations "2.2.0-beta1"]]}})
```

Here is an example test file.

```clojure
(ns expectations-project.core-test
  (:require [expectations :refer :all]
            [expectations.clojure.test :refer [defexpect]]))

(defexpect two
  2 (+ 1 1))

(defexpect three
  3 (+ 1 1))

(defexpect group
  (expect [1 2] (conj [] 1 5))
  (expect #{1 2} (conj #{} 1 2))
  (expect {1 2} (assoc {} 1 3)))
```

And here is the result of running `lein test-refresh`.

```
$ lein test-refresh
*********************************************
*************** Running tests ***************
:reloading (expectations-project.core-test)

FAIL in (group) (expectations_project/core_test.clj:11)
expected: [1 2]
  actual: [1 5] from (conj [] 1 5)

FAIL in (group) (expectations_project/core_test.clj:11)
expected: {1 2}
  actual: {1 3} from (assoc {} 1 3)

FAIL in (three) (expectations_project/core_test.clj:8)
expected: 3
  actual: 2 from (+ 1 1)

Ran 3 tests containing 5 assertions.n
3 failures, 0 errors.

Failed 3 of 5 assertions
Finished at 11:53:06.281 (run time: 0.270s)
```

After some quick edits to fix the test errors and saving the file, here is the output from the tests re-running.

```
*********************************************
*************** Running tests ***************
:reloading (expectations-project.core-test)

Ran 3 tests containing 5 assertions.
0 failures, 0 errors.
:reloading ()

Ran 3 tests containing 5 assertions.
0 failures, 0 errors.

Passed all tests
Finished at 11:53:59.045 (run time: 0.013s)
```

If you're using `expectations` and switch to the new
`clojure.test` compatible syntax, I'd encourage you to start
using
[lein-test-refresh](https://github.com/jakemcc/lein-test-refresh).


[^1]: As of 2016-02-27 `2.2.0` isn't out yet, but `2.2.0-beta1` has been released and has the changes.

[^2]: In fact, you have to use it if you use Leiningen and the new syntax and want your tests to run automatically.
