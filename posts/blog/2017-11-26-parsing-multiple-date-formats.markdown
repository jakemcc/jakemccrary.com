---
dated-url: true
layout: post
title: Parsing multiple date formats with clj-time
date: 2017-11-26 09:52 -0600
comments: true
published: true
description: Need to parse strings with multiple date formats into dates? Here is
  how you can do it.
keywords: clojure, java, joda, jodatime, joda-time, date, clj-time
categories:
- clojure
- java
---

I recently needed to optimize the speed of some Clojure code. After investigating, I identified that a huge number of exceptions were being thrown and handling these was slowing down the process.

The code throwing the exceptions was parsing strings into Joda-Time DateTime objects using the [clj-time](https://github.com/clj-time/clj-time) library.

The code was calling [clj-time.coerce/from-string](https://github.com/clj-time/clj-time/blob/cce58248937bc05452ebfc8b65134961227a554e/src/clj_time/coerce.clj#L33-L38) which calls [clj-time.format/parse](https://github.com/clj-time/clj-time/blob/cce58248937bc05452ebfc8b65134961227a554e/src/clj_time/format.clj#L156-L165). `format/parse` iterates through up to approximately 50 formatters in an attempt to parse whatever string you pass it. If one of these formatters doesn’t parse the string, it throws an exception which `format/parse` catches and ignores before attempting the next formatter.

This was pretty wasteful. This was especially wasteful in the code I was working in since it only needed to handle two different date formats.

Luckily, Joda-Time has a way to build a formatter that handles multiple formats and clj-time provides access to it. Below is code that creates a formatter that handles two different formats.

```clojure
(ns multiple-dates.core
  (:require [clj-time.core :as time]
            [clj-time.format :as time-format]))

(def multi-format
  (time-format/formatter time/utc
                         "YYYY-MM-dd"
                         "YYYY-MM-dd'T'HH:mm:ss.SSSZ"))

(defn parse [s]
  (time-format/parse multi-format s))
```

And below are some examples of using it in the repl.

```clojure
multiple-dates.core> (parse "2017-09-04")
#object[org.joda.time.DateTime 0x5d5e4cd7 "2017-09-04T00:00:00.000Z"]

multiple-dates.core> (parse "2017-09-04T12:11:02.123Z")
#object[org.joda.time.DateTime 0x174f3a5c "2017-09-04T12:11:02.123Z"]

multiple-dates.core> (parse "2017-09-04-12:11:02.123Z")
IllegalArgumentException Invalid format: "2017-09-04-12:11:02.123Z" is malformed at "-12:11:02.123Z"  org.joda.time.format.DateTimeFormatter.parseDateTime (DateTimeFormatter.java:945)
```

Looking back at that code, it seems pretty straightforward. I’ll admit that it took me and my pair a while to figure out how to do this using `clj-time`. I ended up looking at Joda-Time's documentation and implemented this using Java interop before I cracked how to use `clj-time.format/formatter` to do the same thing.