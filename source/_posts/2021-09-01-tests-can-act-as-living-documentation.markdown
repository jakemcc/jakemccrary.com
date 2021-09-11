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
Other tests might be written in response to a discovered bug and, if written first, those tests you know when you've fixed the bug and act as guardrails preventing the reintroduction of that bug.
Tests can also be used to confirm you haven't changed behavior while refactoring.

Tests can also be used as documentation.
Unlike non-executable documentation, tests will always match the implementation's behavior.

An example in a comment or other documentation deserves to be in a test.
Take the following sketch of a Clojure function:

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

The docstring has examples in it to aid humans in understanding its behavior.
These examples are useful!
But they stop being useful and start being dangerous when they stop being accurate.

We can use unit tests to keep examples like this correct.
You can write comments near the assertions letting future readers know about the documentation that needs to be updated if behavior changes.

```clojure
(deftest confobulate-should-ignore-slashes
  ;; If this assertion changes the docstring needs to be updated
  (is (= "//oneOloY" (confobulate "//yolo1"))))

(deftest confobulate-reverses-and-capitalizes
  ;; If this assertion changes the docstring needs to be updated
  (is (= "alice" (confobulate "EcilA"))))
```

Any example in a comment or other non-executable documentation should be an assertion in a unit test.
You've already taken the time to document the behavior; take the time to figure out how to document it in a way that will fail if the behavior changes.

