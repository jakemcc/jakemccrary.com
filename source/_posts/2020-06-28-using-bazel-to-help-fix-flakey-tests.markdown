---
layout: post
title: "Using Bazel to help fix flaky tests"
date: 2020-06-28 15:36:13 -0500
comments: true
published: false
description: Here is a way to use Bazel to help fix flaky tests.
keywords: 'bazel, test, flaky tests'
categories: 
- bazel
---

Flaky tests are terrible.
These are the tests that pass or fail without anything changing in the code.
Often, they pass the majority of the time and fail rarely.
This makes them hard to detect and cause developers to often just run the tests again.

Flaky tests erode your team's confidence in your system.
These tests discourage testing as folks stop seeing tests as something that improves quality and reduces feedback cycles.
With enough flaky tests, the output of test runs starts being ignored as it frequently fails without actual code breakage.

These tests are often hard to fix.
If they were easy to fix then they wouldn't have been flaky in the first place.
The failures are also hard to reproduce, they do not fail consistently.

When you find yourself trying to fix a flaky test you often write a little script to run your tests multiple times in row.
If you are using [Bazel](https://bazel.build/) as your build tool you don't need to this.

Here is an example `bazel`[^1] command for helping you recreate flaky test failures.

[^1]: I've written this while using Bazel 3.2.0. If you are reading this far in the future the flags may have changed.

```bash
bazel test --test_strategy=exclusive --test_output=errors --runs_per_test=50 -t- //...
```

The above command is running all the test targets in a workspace.
`--runs_per_test=50` is telling Bazel to run each test 50 times.
`--test_output=errors` is telling Bazel to only print errors to your console.
`-t-` is a shortcut for `--nocache_test_results` (or `--cache_test_results=no`).
This flag tells Bazel to **not** cache the test results.
`--test_strategy=exclusive` will cause tests to be ran serially.
Without this, Bazel could run your test targets concurrently and if your tests aren't designed for this you may run into other failures.
