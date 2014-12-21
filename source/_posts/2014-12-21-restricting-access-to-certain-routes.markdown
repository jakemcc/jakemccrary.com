---
layout: post
title: "Restricting access to certain routes"
date: 2014-12-21 13:39
comments: true
published: false
categories: 
- clojure
- compojure
- web
---

Recently I've been working on adding authentication and authorization
to a Clojure web service. The project uses
[compojure](https://github.com/weavejester/compojure) for routing and
[friend](https://github.com/cemerick/friend) for authentication and
authorization. My pair and I wanted to restrict access to specific
routes while leaving some routes completely public. It took a few
tries until we figured out how to do this in a way that made us happy.

The rest of this post shows the approximate path we took to our
current solution. It focuses on using friend to restrict access to
specific routes. It does not go into details about adding
authentication to your web service.

Below is an example of the routes before adding authorization checks.

```clojure
(ns example.server
  (:require [compojure.core :refer [GET defroutes] :as compojure]
            [compojure.route :as route]))

(defroutes app
  (GET "/status" _ (status))
  (GET "/cars" _ (fetch-cars))
  (GET "/attributes" _ (fetch-attributes))
  (GET "/drivers" _ (fetch-drivers))
  (route/not-found "NOT FOUND"))
```

We wanted to make `/cars`, `/attributes`, and `/drivers` require that
the request satisfies the `:example.server/user` role. Requesting
`/status` should not require authorization. The first attempt left us
with the following code.

```clojure
(ns example.server
  (:require [compojure.core :refer [GET defroutes] :as compojure]
            [compojure.route :as route]
            [cemerick.friend :as friend]))

(defroutes app
  (GET "/status" _ (status))
  (GET "/cars" _
       (friend/authorize #{::user}
                         (fetch-cars)))
  (GET "/attributes" _
       (friend/authorize #{::user}
                         (fetch-attributes)))
  (GET "/drivers" _
       (friend/authorize #{::user}
                         (fetch-drivers)))
  (route/not-found "NOT FOUND"))
```

The above works but it suffers from repetition. You could write a
macro to minimize the repetition but we thought there must be a better
way.

After reading more of [friend](https://github.com/cemerick/friend)'s
documentation we discovered `friend/wrap-authorize`. This is
middleware that only allows requests through if the request
satisfies the required roles. Our first pass at using
`friend/wrap-authorize` looked like the following example.

``` clojure
(ns example.server
  (:require [compojure.core :refer [GET defroutes] :as compojure]
            [compojure.route :as route]
            [cemerick.friend :as friend]))

(defroutes protected-routes
  (GET "/cars" _ (fetch-cars))
  (GET "/attributes" _ (fetch-attributes))
  (GET "/drivers" _ (fetch-drivers)))

(defroutes app
  (GET "/status" _ (status))
  (friend/wrap-authorize protected-routes #{::user})
  (route/not-found "NOT FOUND"))
```

This is much nicer. The repetition is removed by extracting routes
that require authorization into a seperate `defroutes` and wrapping it
with `friend/wrap-authorize`.

This introduces a subtle bug. A response with status code 404 is no
longer returned if a non-existent resource is requested and the
request is unauthorized. This is because the authorization check
happens _before_ matching a route. friend's documentation warns
against this and suggests using `compojure/context` to scope usage of
`friend/wrap-authorize`. This doesn't solve the problem but it at
least narrows its scope. We can do better.

Compojure
[1.2.0](https://github.com/weavejester/compojure/blob/master/HISTORY.md)
introduced the function `wrap-routes`. `wrap-routes` applies
middleware _after_ a route is matched. By using this we can have all
of the benefits of using `friend/wrap-authorize` without breaking
returning 404 responses.

``` clojure
(ns example.server
  (:require [compojure.core :refer [GET defroutes] :as compojure]
            [compojure.route :as route]
            [cemerick.friend :as friend]))

(defroutes protected-routes
  (GET "/cars" _ (fetch-cars))
  (GET "/attributes" _ (fetch-attributes))
  (GET "/drivers" _ (fetch-drivers)))

(defroutes app
  (GET "/status" _ (status))
  (compojure/wrap-routes protected-routes
                         friend/wrap-authorize
                         #{::user})
  (route/not-found "NOT FOUND"))
```

There we have it. A solution without duplication that still responds
properly to requests for non-existent resources.
`compojure/wrap-routes` is a useful function to know about.
  

