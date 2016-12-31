---
layout: post
title: "Making code fast: Measure what you intend to measure"
date: 2016-12-20 13:15:34 -0600
comments: true
published: false
description: When trying to make code faster, make sure sure you are measuring what you think you are measuring.
keywords: clojure, profiling, tufte, laziness
categories:
- clojure
- performance
---

I’ve spent a significant portion of my career figuring out how to make software run faster. It is a problem I enjoy solving. One of the most important steps in an optimization task is to identify what you are trying to optimize and how you will measure it. Answer these questions wrong and you’ll waste your time solving the wrong problem.

Recently I joined a teammate on a task that involved identifying a bottleneck in a Clojure code base. We knew the code path we needed to optimize and turned to the 
[Tufte](https://github.com/ptaoussanis/tufte) library to take timing measurements. This was my first time using Tufte and, with my tiny amount of usage, I like what I see.

At some point in the process, we had code[^1] that looked similar to the `translate` function below (lines 20-24).

```clojure
(ns bench.core
  (:require [clojure.string :as string]
            [taoensso.tufte :as tufte]))

(defn raw->maps [lines]
  (map (fn [line]
         (zipmap [:a :b :c]
                 (map (fn [s] (Long/parseLong s))
                      (string/split line #"\|"))))
       lines))

(defn summarize [maps]
  (reduce (fn [r m]
            (-> r
                (update :a (fnil + 0) (:a m))
                (update :b (fnil + 0) (:b m))
                (update :c (fnil + 0) (:c m))))
          maps))

(defn translate [lines]
  (tufte/profile {}
                 (let [maps (tufte/p ::raw->maps (raw->maps lines))
                       summary (tufte/p ::summarize (summarize maps))]
                   summary)))
```

Here is some Tufte output from running some data through `translate`.

```
                  pId      nCalls       Min        Max       MAD      Mean   Time% Time
:bench.core/summarize           1   346.0ms    346.0ms       0ns   346.0ms     100 346.0ms
:bench.core/raw->maps           1    2.46µs     2.46µs       0ns    2.46µs       0 2.46µs
           Clock Time                                                          100 346.05ms
       Accounted Time                                                          100 346.0ms
```

Notice anything surprising with the output?[^2]

It surprised me that `raw->maps` took such a tiny amount of time compared to the `summarize` function. Then I realized that we had forgotten about Clojure’s lazy sequences. `summarize` is taking so much of the time because `raw->maps` is just creating a lazy sequence; all the work of realizing that sequence happens in `summarize`. By wrapping the call to `raw->maps` with a `doall` we were able to get the time measurements we intended.

This example demonstrates an important lesson. When you are profiling code, make sure you are measuring what you think you are measuring. This can be challenging in languages, such as Clojure, that have a concept of laziness. Reflect on your measurement results and perform a gut check that the results make sense with what you intended to measure. If anything feels off, confirm that you’re measuring what you meant to measure.

[^1]: Example built using clojure 1.8.0 and tufte 1.1.1. Also, sorry for the terrible names of functions. I was drawing a blank when coming up with this example.

[^2]: Imagine this output having 10 more lines in it. Now imagine it having 20. It starts getting quite a bit more difficult to notice oddities as more and more lines get added to this output. Try not to overwhelm yourself by having too much output.
