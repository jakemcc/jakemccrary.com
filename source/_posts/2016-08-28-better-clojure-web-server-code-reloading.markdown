---
layout: post
title: "Better Clojure web server code reloading"
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
reload changed code.

That desire happened because my team was running into reloading
problems using ring's `wrap-reload` middleware. Unforunately these
problems happened prior to January 2016 and, since I didn't write this
post back then, I've since forgotten what the problems
were. Regardless, this project has been being used since the beginning
of this year and has helped make my team's development workflow
smoother.

### Usage 

If you'd like to give it a shot, then add the [latest version](https://clojars.org/com.jakemccrary/reload) to your project.clj. 

[![Clojars Project](https://img.shields.io/clojars/v/com.jakemccrary/reload.svg)](https://clojars.org/com.jakemccrary/reload)

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
