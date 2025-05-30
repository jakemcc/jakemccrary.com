---
dated-url: true
layout: post
title: Tracking changes to a Reagent atom
date: 2018-06-15 09:05 -0500
comments: true
published: true
description: Debug your ClojureScript Reagent UIs by tracking changes to your applications
  global db.
keywords: clojurescript, clojure, reagent
categories:
- clojurescript
- reagent
---

I was recently having some difficulty debugging a problem in a ClojureScript single page application.
The SPA was implemented using [reagent](https://reagent-project.github.io/)[^1].

[^1]: This particular project is nearly four years old and has had many hands on it over the years. Working in it reminds me of how useful re-frame is on larger applications like this one.

This interface stores most of its state in a global `reagent.core/atom` called `db`.
To debug the problem, I thought it would be useful to track how the global state changed as I interacted with the interface.
How do we do that?

For the rest of this article, pretend that `(require '[reagent.core :as reagent])` has been executed.

First, let's define `db-history` in the same namespace as the global `reagent/atom`, `db`.
This is where we'll collect the changes to `db`.

```clojure
(ns ui.data
  (:require [reagent.core :as reagent]))

(defonce db (reagent/atom {:app/current-page :offer-list}))

(defonce db-history (atom []))
```

Next, let's write a function called `aggregate-state`.
This function grabs the current value in `db` and `conj`s it onto `db-history`.
It also limits the history to the most recent 101 states.

```clojure
(defn aggregate-state []
  (let [d @db]
    (swap! db-history (fn [hist]
                        (-> (take 100 hist)
                            vec
                            (conj d))))))
```

Now we need to invoke `aggregate-state` whenever `db` changes.
We can do this using `reagent/track`.
`reagent/track` takes a function and optional arguments and invokes that function whenever a `reagent/atom` that function depends on changes.

`reagent/track!` is similar except it immediately invokes the function instead of waiting for the first change.
We can use it to cause `aggregate-state` to get called whenever `db` changes.

```clojure
(defonce db-history-logger (reagent/track! aggregate-state))
```

Now history of the global state is being tracked.
But we need a way to access it.
Below is what I ended up writing.
When you call `ui.data.history()` in Chrome's JavaScript console, it returns an object you can click on to explore.
If you pass in strings as arguments to `history` then it only selects some of the data from the global `db` and history.

```clojure
(defn ^:export history [& args]
  (let [d @db
        k (if (seq args)
            (map keyword args)
            (keys d))]
    (clj->js {:history (mapv (fn [x] (select-keys x k)) @db-history)
              :current (select-keys d k)})))
```

It only took about fifteen lines of code to gain a view of our application's state changes over time.
This view helped me solve my problem.
Hopefully it will help you too.