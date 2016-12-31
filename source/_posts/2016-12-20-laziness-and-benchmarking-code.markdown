---
layout: post
title: "Laziness and benchmarking code"
date: 2016-12-20 13:15:34 -0600
comments: true
published: false
description: PUT SUMMARY HERE 
keywords: 'csv, keywords, here'
categories: 
---

I've spent a significant portion of my career figuring out how to make software run faster. This has ranged from reducing the latency of automated trading systems as they react to triggers to making web applications respond faster to requests to making large periodic tasks finish their bulk processing quicker. Performance optimization is something I really enjoy attacking. One of the most important steps in an optimization task is to identify what you are trying to optimize and how you will measure it. Answer these questions wrong and you'll waste your time solving the wrong problem.

Recently I was involved in a task that involved identifying a bottleneck in a Clojure code base. I joined a teammate on the task and we took a shot at solving the problem. We knew the code path we needed to optimize and turned to the [Tufte](https://github.com/ptaoussanis/tufte) library to take timing measurements of our code. This was my first time using Tufte and, with my admittedly tiny amount of usage, I like what I see.

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
:bench.core/raw->maps           1    2.46μs     2.46μs       0ns    2.46μs       0 2.46μs
           Clock Time                                                          100 346.05ms
       Accounted Time                                                          100 346.0ms
```

Notice anything surprising with the output?[^2]

It surprised me that `raw->maps` took such a tiny amount of time compared to the `summarize` function. Then I realized that we had forgotten about Clojure's lazy sequences. `summarize` is taking so much of the time because `raw->maps` is just creating a lazy sequence; all of the work of realizing that lazy sequence is done in `summarize`. By wrapping the call to `raw->maps` with a `doall` we were able to get the time measurements we intended.

This somewhat long-winded example demonstrates an important lesson. When you are profiling code, make sure you are measuring what you think you are measuring. This can be challenging in languages, such as Clojure, that have a concept of laziness.

[^1]: Example built using clojure 1.8.0 and tufte 1.1.1. Also, sorry for the terrible names of functions. I was drawing a blank when coming up with this example.
