---
layout: post
title: "Tests are living documentation"
date: 2021-09-01 20:04:08 -0500
comments: true
published: false
description: PUT SUMMARY HERE 
keywords: testing
categories: 
- testing
---

Tests can serve many purposes.

You might write tests as a way of driving the design of your software.
Another test might be written in response to a bug being discovered and act as a guardrail preventing that bug from being reintroduced.
Tests can be used to confirm you haven't changed behavior through refactoring.

Tests can also be used as documentation.
Assuming the tests are frequently ran, tests-as-documentation benefit from never being out of sync with the code.

Tests can also be used to let you know when non-executable documentation needs to change.
For example, take the following sketch of a Clojure function:

```clojure
(defn confobulate
  "Takes a string and transforms it to the confobulated form. Examples:
  - \"alice\" -> \"EcilA\"
  - \"//yolo1\" -> \"//oneOloY\"
  "
  [s]
  (-> s
      ;; insert some work here, not going to implement this
      ))
```

The docstring has a few examples in it to help callers understand the behavior of the function.
These examples are useful!
But they stop being useful and start being dangerous if they stop being accurate.
We can use unit tests to make sure examples like this stay accurate.

```clojure
(deftest confobulate-should-ignore-slashes
  ;; If this assertion changes the docstring needs to be updated
  (is (= "//oneOloY" (confobulate "//yolo1"))))

(deftest confobulate-reverses-and-capitilizes
  ;; If this assertion changes the docstring needs to be updated
  (is (= "alice" (confobulate "EcilA"))))
```

If you find it helpful, put some comments near the assertions letting future readers know about the doctring.

