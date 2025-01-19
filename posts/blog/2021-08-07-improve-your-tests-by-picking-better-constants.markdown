---
layout: post
title: Improve your tests by picking better constants
date: 2021-08-07 19:58 -0500
comments: true
published: true
description: The constants you put in your test code can improve its readability.
  You should put thought into them.
keywords: clojure, test, unit test, readability, legibility
categories:
- testing
---

The constants you use in unit tests matter.
Like test and variable names, they can improve the readability of your code and make it easier to understand test failures.

Imagine the following.

A new developer joins your team and asks a question about how the code resolves config values.
You are unsure of the details so you pair up with the new teammate to dig into the code.

You know the codebase uses a relatively simple key-value pair concept for configuration.
It reads keys and values from a known files and, based on some rules, either ignores or overrides values when keys are duplicated across files.

`config-value` is the function that looks up the value for a particular configuration key, represented as a `string`.
This function takes three arguments: an in-memory representation of the configuration files, the key to lookup, and the mode to operate in.
You know the mode is important in influencing how config resolution works but you don't remember the details.

Luckily for you and your pair, the codebase has plenty of unit tests.
The two of you dive in and look at some tests, hoping to understand how config resolution works.

```clojure
(def config {"scratch.conf" {"a" "1"}

             "development.conf" {"a" "2"
                                 "b" "2"}

             "application.conf" {"a" "3"
                                 "b" "3"
                                 "c" "3"}})

(deftest handles-overrides-in-dev-mode
  (is (= "1" (config-value config "a" :dev)))
  (is (= "2" (config-value config "b" :dev)))
  (is (= "3" (config-value config "c" :dev))))

(deftest handles-overrides-in-prod-mode
  (is (= "3" (config-value config "a" :prod)))
  (is (= "3" (config-value config "b" :prod)))
  (is (= "3" (config-value config "c" :prod))))
```

It is great that these tests exist but they could be clearer.
They aren't terrible but you have to work a bit understand what is happening.

When reading `(= "2" (config-value config "b" :dev))`, what does `"2"` represent?
What does `"b"` mean?
You have to either keep the value of `config` in your brain or keep glancing up in the file to recall what it is.

This isn't great.
This adds cognitive overhead that doesn't need to be there.

There are a few ways these tests could be improved
One way is through using better constants.
Let's do a quick rewrite.

```clojure
(def config {"scratch.conf" {"in dev+app+scratch" "from scratch"}

             "development.conf" {"in dev+app+scratch" "from development"
                                 "in dev+app" "from development"}

             "application.conf" {"in dev+app+scratch" "from application"
                                 "in dev+app" "from application"
                                 "in app" "from application"}})

(deftest handles-overrides-in-dev-mode
  (is (= "from scratch" (config-value config "in dev+app+scratch" :dev)))
  (is (= "from development" (config-value config "in dev+app" :dev)))
  (is (= "from application" (config-value config "in app" :dev))))

(deftest handles-overrides-in-prod-mode
  (is (= "from application" (config-value config "in dev+app+scratch" :prod)))
  (is (= "from application" (config-value config "in dev+app" :prod)))
  (is (= "from application" (config-value config "in app" :prod))))
```

These are the same tests but with different constants.
Those constants make a huge difference.
This change has made the tests more legible.
You no longer need to remember the value of `config` or keep glancing up at it to understand the assertions in a test.

You can read `(= "from development" (config-value config "in dev+app" :dev))` and have a pretty solid idea that you are looking up a key found in both `development.conf` and `application.conf` and while in `:dev` mode expect the value from `development.conf`.

The new constants provide clues about what the test expects.
You can read and understand the assertions without keeping much state in your head.

This increases the legibility of the tests and is useful when a test fails.
Which of the following is clearer?

```
FAIL in (handles-overrides-in-dev-mode)
expected: "2"
  actual: "3"
    diff: - "2"
          + "3"
```

```
FAIL in (handles-overrides-in-dev-mode)
expected: "from development"
  actual: "from application"
    diff: - "from development"
          + "from application"
```

The second one is clearer.
You can read it and form a hypothesis about what might be broken.

Well chosen constants reduce the state a person needs to keep in their head.
This makes tests easier to understand.
Good constants also make test failures easier to understand.
Just like good variable names, good constants increase the readability of our tests.

It is well worth placing some extra thought into the constants found in your tests.
