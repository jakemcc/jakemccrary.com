---
layout: post
title: "Tests are documentation"
date: 2021-09-01 20:04:08 -0500
comments: true
published: false
description: PUT SUMMARY HERE 
keywords: 'csv, keywords, here'
categories: 
---

The tests targeting the software you care about can serve different purposes.

Some tests help guide your design.

Other tests might act as guardrails.
They provide feedback that code works as expected.
They break if you make a mistake while refactoring.

You might write another test to protect against a bug from being reintroduced.

There are also exploratory tests.
A collegue might write some tests while exploring the functionality of a piece of code.
Tests you might write to help you better understand the behavior of some code.

Tests can also serve as documentation.
They show how a piece of code behaves under certain conditions with certain inputs.

Below is an example function you might come across in a codebase.

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

The documentation includes some examples in it.
Hopefully these are helpful examples to the caller and potential readers of the function.

But this documentation is effectively a comment.
There is nothing that forces it to be in-sync with the implementation.

This type of documentation needs to exist in a test.
This test needs to be ran regularly.
Since the test is being ran regularly, it will fail when the implementation changes and helpful documentation stops being helpful.

```clojure
(deftest confobulate-should-ignore-slashes
  (is (= "//oneOloY" (confobulate "//yolo1"))))

(deftest confobulate-reverses-and-capitilizes
  (is (= "alice" (confobulate "EcilA"))))
```

If you feel the need, you can even add a comment near the tests reminding you to update the function's documentation.
Alternatively, you could also change the documentation to point towards the tests!
Then it will never be out of date.
