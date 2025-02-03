---
dated-url: true
layout: post
title: Using Bazel to help fix flaky tests
date: 2020-06-28 18:28 -0500
comments: true
published: true
description: Here is a way to use Bazel to help fix flaky tests.
keywords: bazel, test, flaky tests
categories:
- bazel
- testing
---

Flaky tests are terrible.
These are tests that pass or fail without anything changing in the code.
They often pass the majority of the time and fail rarely.
This makes them hard to detect and cause developers to often just run the tests again.

Flaky tests erode your team's confidence in your system.
They cause folks to get in the habit of not trusting the output of tests.
This discourages people from writing tests as they stop seeing them as something that improves quality and instead view them as a drag on productivity.

Flaky tests are often hard to fix.
If they were easy to fix, they wouldn't have been flaky in the first place.
One difficulty in fixing them is that the failures are often hard to reproduce.

Often, the first step in fixing a flaky test is to write a script to run the tests multiple times in a row.
If you are using [Bazel](https://bazel.build/) as your build tool you don't need to write this.

Here is an example `bazel`[^1] command for helping you recreate flaky test failures.

[^1]: I've written this while using Bazel 3.2.0. If you are reading this far in the future the flags may have changed.


`bazel test --test_strategy=exclusive --test_output=errors --runs_per_test=50 -t- //...`


The above command is running all the test targets in a workspace and each flag is important.

- `--runs_per_test=50` is telling Bazel to run each test 50 times.
- `--test_output=errors` is telling Bazel to only print errors to your console.
- `-t-` is a shortcut for `--nocache_test_results` (or `--cache_test_results=no`).
This flag tells Bazel to **not** cache the test results.
- `--test_strategy=exclusive` will cause tests to be run serially.
Without this, Bazel could run your test targets concurrently and if your tests aren't designed for this you may run into other failures.

Flaky tests are terrible and you should try not to have them.
Try your best to have reliable tests.