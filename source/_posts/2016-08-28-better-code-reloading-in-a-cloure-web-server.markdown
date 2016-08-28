---
layout: post
title: "Better code reloading in a Clojure web server"
date: 2016-08-28 13:36:37 -0500
comments: true
published: false
description: Here is a better alternative to ring/wrap-reload.
keywords: 'clojure, ring, wrap-reload, reload'
categories: 
- clojure
---

I'm announcing the release of
[com.jakemccrary/reload](https://github.com/jakemcc/reload). This tiny
library was created in January 2016 as a result of wanting a ring
middleware that uses
[org.clojure/tools.namespace](https://github.com/clojure/tools.namespace)
to reload changed code.

That desire happened because my team was running into reloading
problems using ring's `wrap-reload` middleware. Unfortunately these
problems happened about nine months ago and, since I didn't write this
post back then, I've since forgotten these problems. Regardless, this
project has been used since the beginning of this year and has helped
make my team's development workflow smoother. If you are running into
problems it might help you too.

### Usage 

If you'd like to give it a shot, then add the [latest version](https://clojars.org/com.jakemccrary/reload) (at the time of writing `[com.jakemccrary/reload "0.1.0"]`) to your project.clj. 

Require `com.jakemccrary.middleware.reload` and wrap your handler with `wrap-reload`.

```clojure
(ns example
  (:require
   ;; more deps
   [com.jakemccrary.middleware.reload :as reload]))

;; wherever you are setting up your middleware stack
(reload/wrap-reload routes)
```

`reload/wrap-reload` optionally takes a list of directories to monitor
as a second parameter. By default it reloads the `src` directory.
