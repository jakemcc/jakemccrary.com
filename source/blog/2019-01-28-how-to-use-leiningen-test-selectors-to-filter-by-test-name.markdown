---
dated-url: true
layout: post
title: How to use Leiningen test selectors to filter by test name
date: 2019-01-28 21:27 -0600
comments: true
published: true
description: 
keywords: clojure, leiningen, test selectors, filter, test-refresh
categories:
- clojure
- leiningen
- lein-test-refresh
- testing
---

<!-- Estimate: 30 minutes -->
<!-- First draft: 38 minutes -->
<!-- First edit: 13 minutes -->
<!-- Second edit: 8 minutes -->

Leiningen test selectors are great.
They allow you to filter what tests run by applying a function to the test's metadata.
If that function returns a truthy value then that test will run.
[lein-test-refresh](https://github.com/jakemcc/test-refresh/blob/master/CHANGES.md#040) supports them and even includes a built in one for its [focus feature](https://github.com/jakemcc/test-refresh#built-in-test-narrowing-test-selector).

I was recently [asked](https://github.com/jakemcc/test-refresh/issues/78) if test-refresh could support filtering tests using a regular expression against the name of a namespace or test.
Lucky for me, test-refresh already supports this because of its support of test selectors.

Most of the examples of Leiningen test selectors show very simple functions that look for the existence of a keyword in the metadata.
We can do more than that.
We can write a predicate that does whatever we want with the metadata.

To take a look at a test's metadata, I generated a new project and looked at the generated default test file.

```clojure
(ns selector.core-test
  (:require [clojure.test :refer :all]
            [selector.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
```

I then used my repl and to see what metadata was on the test.

```clojure
selector.core-test> (meta #'a-test)
{:test #function[selector.core-test/fn--17267],
 :line 5,
 :column 1,
 :file "/Users/jake/src/jakemcc/blog-examples/selector/test/selector/core_test.clj",
 :name a-test,
 :ns #namespace[selector.core-test]}
```

Given the metadata above, I wrote the selector below which lets us select only integration tests.

```clojure
:test-selectors {:integration (fn [m]
                                (or (clojure.string/includes? (str (:ns m))
                                                              "integration")
                                    (clojure.string/includes? (str (:name m))
                                                              "integration")))}
```

You could write the above code is many different ways.
Whatever you write, it needs to look for the existence of `integration` in either the test's name or namespace.

If you wanted to make `lein test` or `lein test-refresh` only run non-integration tests you can add a default test selector to the project.clj.

```clojure
:test-selectors {:default (fn [m]
                            (not (or (clojure.string/includes? (str (:ns m))
                                                               "integration")
                                     (clojure.string/includes? (str (:name m))
                                                               "integration"))))
                 :integration (fn [m]
                                (or (clojure.string/includes? (str (:ns m))
                                                              "integration")
                                    (clojure.string/includes? (str (:name m))
                                                              "integration")))}
```

Enjoy!
I hope this example helps you run a subset[^1] of your Clojure tests through Leiningen test selectors.

[^1]: Running a subset of your tests can be helpful and test-refresh has a few features that help you do that. If you can, I'd still recommend making all your tests fast enough to run them all the time.