---
layout: post
title: "Measuring aggregate performance in Clojure"
date: 2017-09-29 08:48:01 -0500
comments: true
published: false
description: Short Clojure code for measuring the aggregate performance of chunks of code.
keywords: clojure, profiling, laziness
categories: 
- clojure
- performance
---

Last time I needed to speed up some code, I wrote a Clojure macro that recorded the aggregate time spent executing the code wrapped by the macro. Aggregate timings were useful since the same functions were called multiple times in the code path we were trying to optimize. Seeing total times made it easier to identify where we should spend our time.

Below is the namespace I temporarily introduced into our codebase.

```clojure
(ns metrics)

(defn msec-str
  "Returns a human readable version of milliseconds based upon scale"
  [msecs]
  (let [s 1000
        m (* 60 s)
        h (* 60 m)]
    (condp >= msecs
      1 (format "%.5f msecs" (float msecs))
      s (format "%.1f msecs" (float msecs))
      m (format "%.1f seconds" (float (/ msecs s)))
      h (format "%02dm:%02ds" (int (/ msecs m))
                (mod (int (/ msecs s)) 60))
      (format "%dh:%02dm" (int (/ msecs h))
              (mod (int (/ msecs m)) 60)))))

(def aggregates (atom {}))

(defmacro record-aggregate
  "Records the total time spent executing body across invocations."
  [label & body]
  `(do
     (when-not (contains? @aggregates ~label)
       (swap! aggregates assoc ~label {:order (inc (count @aggregates))}))
     (let [start-time# (System/nanoTime)
           result# (do ~@body)
           result# (if (and (seq? result#)
                            (instance? clojure.lang.IPending result#)
                            (not (realized? result#)))
                     (doall result#)
                     result#)
           end-time# (System/nanoTime)]
       (swap! aggregates
              update-in
              [~label :msecs]
              (fnil + 0)
              (/ (double (- end-time# start-time#)) 1000000.0))
       result#)))

(defn log-times
  "Logs time recorded by record-aggregate and resets the aggregate times."
  []
  (doseq [[label data] (sort-by (comp :order second) @aggregates)
          :let [msecs (:msecs data)]]
    (println "Executing" label "took:" (msec-str msecs)))
  (reset! aggregates {}))
```

`record-aggregate` takes a label and code and times how long that code takes to run. If the executed code returns an unrealized lazy sequence, it also evaluates the sequence[^1].

[^1]: Sea [Measure what you intend to measure](/blog/2016/12/31/measure-what-you-intended-to-measure/)

Below is an example of using the above code. When we used it, we looked at the code path we needed to optimize and wrapped chunks of it in `record-aggregate`. At the end of the calculations, we inserted a call to `log-times` so timing data would show up in our logs.


```clojure
(ns work
  (:require [metrics :as m]))

(defn calculation [x]
  (m/record-aggregate ::calculation
                      (Thread/sleep (+ 300 (rand-int 60)))
                      x))

(defn work [x]
  (m/record-aggregate ::work
                      (repeatedly 10 (fn []
                                       (Thread/sleep 5)
                                       x))))

(defn process-rows [rows]
  (let [rows (m/record-aggregate ::process-rows
                                 (->> rows
                                      (mapv calculation)
                                      (mapcat work)))]
    (m/log-times)
    rows))
```

Now, when `(process-rows [:a :a])` is called output similar to below is printed.

```
Executing :work/process-rows took: 780.9 msecs
Executing :work/calculation took: 664.6 msecs
Executing :work/work took: 115.8 msecs
```

Using this technique, we were able to identify slow parts of our process and were able to optimize those chunks of our code. There are potential flaws with measuring time like this, but they were not a problem in our situation[^2].

[^2]:  See [Nanotrusting the Nanotime](https://shipilev.net/blog/2014/nanotrusting-nanotime/)
