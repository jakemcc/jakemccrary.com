---
layout: post
title: "The usefulness of Clojure's cond->"
date: 2016-04-09 15:10:42 -0500
comments: true
published: false
description: Clojure's cond-> (and cond->>) is a pretty useful function.
keywords: clojure, cond->, functional programming, cond->>
categories: 
- clojure
---

Clojure's
[`cond->`](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/cond-%3E)
(and `cond->>`) is a versatile function. It isn't a new function, it
has been around since version 1.5, but I finally discovered and
started using it sometime last year. It isn't a function you'll use
every day but when it is useful you definitely end up with better
code.

### What is `cond->`? ###

Let's start by looking at the docstring.

```
Usage: (cond-> expr & clauses)

Takes an expression and a set of test/form pairs. Threads expr (via ->)
through each form for which the corresponding test
expression is true. Note that, unlike cond branching, cond-> threading does
not short circuit after the first true test expression.
```

So what does the docstring mean? Let's break it down with an example.

``` clojure
(cond-> 10
  false inc)
=> 10
```

In the above example `10` is the `expr` mentioned in the docstring and
everything after it are the `clauses`. Each clause is a pair made up
of a test and a form. The value `false` is the form and the function
`inc` is the form. Since the test evaluates to a false value, the
expression is not threaded into the form. As a result the
original expression, `10`, is returned.

Let's look at an example with a truthy test.

``` clojure
(cond-> 10
  true (- 2)
=> 8
```

Here `10` is still the starting expression. The single clause has a
test that evaluates to true, so the expression is threaded into the
first position of the form `(- 10 2)`. The result of this is `8` and
that is returned.

Next is an example of a `cond->` with multiple clauses. Explanations
are inline with the code.

``` clojure
(cond-> 10 ; start with 10
  ;; test evaluates to true, so apply inc to 10. Current value is now 11.
  true inc

  ;; (zero? 1) evaluates to false, do not perform action. Current value stays 11.
  (zero? 1) (+ 2)

  ;; (pos? 4) evaluates to true, thread 11 into first position of form.
  (pos? 4) (- 5))
=> 6 ; The result of (- 11 5) is 6.
```

If you understand the above example then you have a good grasp of `cond->`. But when is this functionality useful?

### When do I use cond->? ###

Looking through the codebases I work on I almost exclusively see us
using `cond->` with the initial expression being a hash-map. It is
primarily be used in situations where we want to want to selectively
`assoc`, `update`, or `dissoc` something from a map.

If `cond->` didn't exist you would write something similar to below to
selectively `assoc` something into a map.

``` clojure
(if (some-pred? q)
  (assoc m :a-key :a-value)
  m)
```

You can rewrite the above with `cond->`.

``` clojure
(cond-> m
  (some-pred? q) (assoc :a-key :a-value))
```

One of the projects I work on uses
[honeysql](https://github.com/jkk/honeysql) for expressing some of the
SQL queries as Clojure data structures. Starting with a base query and
selectively modifying it using `cond->` has helped with code
clarity. An example of doing this is below.

``` clojure
(defn query [params]
  (let [and-clause (fnil conj [:and])
        base-query {:select [:name :job]
                    :from [:person]}]
    (cond-> base-query
      (:job params) (update :where and-clause [:= :job (:job params)])
      (:name params) (update :where and-clause [:= :name (:name params)])
      (:min-age params) (update :where and-clause [:> :age (:min-age params)]))))
```

Hopefully this gives you a taste of `cond->`. It has been a useful
addition to my codebases. It has a place in every Clojure developer's
toolbox.
