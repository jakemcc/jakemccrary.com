---
layout: post
title: "Preventing duplicate long-running invocations in Clojure"
date: 2018-04-13 14:24:45 -0500
comments: true
published: false
description: Here is a quick Clojure solution to ensuring multiple invocations of a function don't result in duplicate work.
keywords: 'clojure'
categories: 
- clojure
---

A couple months ago I was looking into a problem and noticed that
there was a situation where an expensive operation could be running
simultaneously multiple times. This was wasteful.

This operation happened on a timer and could also be triggered by a power
user through the UI. A power user could accidentally (or purposefully)
mash on a UI button and cause the instance they're interacting with
to grind to a halt[^1].

[^1]: OK, not really grind to a halt, but consume unnecessary resources.

It was pretty easy to prevent. All I needed to introduce was an
`atom` and lean on `compare-and-set!`. `compare-and-set!` is a pretty
neat function (and concept found in many languages). Here is the docstring:

> Atomically sets the value of atom to newval if and only if the
> current value of the atom is identical to oldval. Returns true if
> set happened, else false

Basically, `compare-and-set!` changes the value of an atom only if it
starts from a specified value and returns a boolean letting you know
if it did. 

To prevent an operation from running multiple times, introduce an atom
and wrap calling the operation in a conditional using
`compare-and-set!`.  After doing the work, be sure to `reset!` your
atom back to the starting value.

Below is the code.

```clojure
(defonce running? (atom false))

(defn- expensive-operation!' []
  ;; do work
  )

(defn expensive-operation! []
  (when (compare-and-set! running? false true)
    (try
      (expensive-operation!')
      (finally
        (reset! running? false)))))
```
