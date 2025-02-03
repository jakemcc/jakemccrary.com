---
dated-url: true
layout: post
categories: [code, clojure]
comments: true
date: 2010-06-06
title: Inserting values into a nested map in Clojure
---

Recently I was writing some Clojure with a coworker and we needed to insert values into a nested map structure. Our first solution (and example of using it at the repl) looked something like this.

``` clojure
(defn add-to-cache [cache key1 key2 data]
  (let [entry (get cache key1 {})
        new-entry (assoc entry key2 data)]
    (assoc cache key1 new-entry)))

user> (-> (add-to-cache {} :chicago :lakeview :jake)
          (add-to-cache :sf :mission :dan)
          (add-to-cache :chicago :wickerpark :alex))
{:sf {:mission :dan}, :chicago {:wickerpark :alex, :lakeview :jake}}
```


This worked but seemed overly verbose for doing what (in our minds) should have been a simple operation. After some digging around in the docs we found the function `assoc-in`. This useful function allowed us to greatly simplify the code.

``` clojure
(defn add-to-cache [cache key1 key2 data]
  (assoc-in cache [key1 key2] data))

user> (-> (add-to-cache {} :chicago :lakeview :jake)
          (add-to-cache :sf :mission :dan)
          (add-to-cache :chicago :wickerpark :alex))
{:sf {:mission :dan}, :chicago {:wickerpark :alex, :lakeview :jake}}
```

Much simpler and easier to read. The next person to look at the code will be able to quickly skim and tell what the code is doing.

`assoc-in` can also be used with nested associative structures like vectors.

``` clojure
user> (assoc-in [[0 1] [:a :b]] [0 1] :z)
[[0 :z] [:a :b]]
user> (assoc-in [[0 1] [:a :b]] [1 1] :z)
[[0 1] [:a :z]]
```

Hopefully this post makes searching for how to insert into nested maps slighly easier for the next person who thinks there must be a better way for doing this.