---
layout: post
title: "Using Clojure macros for nicer error handling"
date: 2017-12-30 18:56:39 -0600
comments: true
published: false
description: Here is one way to have better error handling by using Clojure macros
keywords: 'clojure, macro, error handling, monad'
categories: 
- clojure
---

In July 2017, I found myself editing some Clojure code that looked approximately like this.

```clojure
(defn validate-required-fields [params]
  (when-not (contains? params :source)
    "Missing source field"))

(defn validate-invariants [params]
  (when (>= (:lower params) (:higher params))
    "lower field must be smaller than higher"))

;; route handler taken out of other routes
(GET "/event-redirect/:event_type" request []
  (let [params (:params request)]
    (if-let [field-error (validate-required-fields params)]
      {:status 400 :body field-error}
      (if-let [invariant-error (validate-invariants params)]
        {:status 400 :body invariant-error}
        (publish-and-redirect params)))))
```

This route handler validates its inputs, and if they fail validation,
then it returns an error response. I found this pretty ugly. This
small chunk of code has numerous `if` branches and quite a bit of
nesting. All of this makes it hard to read and hurts understanding.

While adding a new feature to it, I remembered some code I wrote with
[Case](https://github.com/snoe) back in late 2015. Back then we were
working on Lumanu and wrote a Clojure macro that we called
`halt-on-error->>`. This macro worked similarly to `->>`, except it
allowed any step in the processing pipeline to halt execution and
trigger an error handler. We were working on a web crawler at the
time, and this macro significantly improved the readability of our
data processing pipeline. There was a lot of error handling code
throughout the web crawler, and this macro helped keep it readable.

I realized that using a similar macro would make this code easier to
follow. I recreated `halt-on-error->>` to allow any form to cause it
to return early. The above code could then be written like below.

```clojure
(defn validate-required-fields [params]
  (if (contains? params :source)
    params
    (exec/halt {:status 400 :body "Missing source field"})))

(defn validate-invariants [params]
  (if (< (:lower params) (:higher params))
    params
    (exec/halt {:status 400 :body "lower field must be smaller than higher"})))

(GET "/event-redirect/:event_type" request []
  (exec/halt-on-error->> request
                         :params
                         validate-required-fields
                         validate-invariants
                         publish-and-redirect))
```

Once you understand `halt-on-error->>`, this chunk of
code is much easier to read.
    
Let's implement `halt-on-error->>`.

## Implementing `halt-on-error->>`

Here are some tests for that specify how `halt-on-error->>` should work.

```clojure
(ns halt.execution-test
  (:require  [halt.execution :as exec]
             [clojure.test :refer :all]))

(def produce-error (constantly (exec/halt {:x "foobar"})))

(defn success-fn
  "Weird function that appends suffix to s"
  [suffix s]
  (str s suffix))

(deftest single-step
  (is (= "first" (exec/halt-on-error->> (success-fn "first" "")))))

(deftest two-steps-with-no-error
  (is (= "firstsecond" (exec/halt-on-error->> (success-fn "first" "")
                                              (success-fn "second")))))

(deftest error-as-first-step
  (is (= {:x "foobar"} (exec/halt-on-error->> (produce-error))))
  (is (= {:x "foobar"} (exec/halt-on-error->> (produce-error)
                                              (success-fn "first")))))

(deftest error-after-first-step
  (is (= {:x "foobar"} (exec/halt-on-error->> (success-fn "first" "")
                                              (produce-error)
                                              (success-fn "second")))))

(deftest works-with-anonymous-functions
  (is (= 1 (exec/halt-on-error->> (success-fn "first" "")
                                  ((fn [x] (exec/halt 1)))))))
```

Below is an implementation of `halt-on-error->>`.

```clojure
(ns halt.execution)

(defrecord Stopper [x])

(defn halt [data]
  (Stopper. data))

(defmacro halt-on-error->> [form & forms]
  (let [g (gensym)
        pstep (fn [step] `(if (instance? Stopper ~g) ~g (->> ~g ~step)))]
    `(let [~g ~form
           ~@(interleave (repeat g) (map pstep forms))]
       (if (instance? Stopper ~g)
         (.x ~g)
         ~g))))
```

So what is this macro doing? First, it uses `gensym` to get a symbol
with a unique name and stores this in `g`. It then defines a helper
function called `pstep` for use in the code generation part of the
macro.

This macro generates a `let` block that repeatedly executes a form and
assigns the return value back to `g`. `g` is then checked to confirm
execution should continue before it is threaded into the next form. If
`g` is ever an instance of a `Stopper`, execution halts and the value
wrapped in the `Stopper` is returned.

Looking at an expanded version of a macro can be easier to understand
than a written explanation. Below is a macroexpanded version of one of
the tests.

```clojure
;; What is being expanded
(macroexpand-1 '(exec/halt-on-error->> (success-fn "first" "")
                                       (produce-error)
                                       (success-fn "second")))

;; The expansion
(let [G__15365 (success-fn "first" "")
      G__15365 (if (instance? halt.execution.Stopper G__15365)
                 G__15365
                 (->> G__15365 (produce-error)))
      G__15365 (if (instance? halt.execution.Stopper G__15365)
                 G__15365
                 (->> G__15365 (success-fn "second")))]
  (if (instance? halt.execution.Stopper G__15365)
    (.x G__15365)
    G__15365))
```

Looking at that expansion, you can see how we are using a `let` block
to repeatedly assign to the same symbol and we check that return value
before executing the next stop.

This isn't a new pattern. There are
[libraries](https://github.com/kumarshantanu/promenade) that implement
similar ideas. At IN/Clojure 2018, Varun Sharma gave a
[talk](https://www.slideshare.net/VarunSharma143/elegant-errorhandling-for-a-more-civilized-age)
about how this cleaned up their code. You can even get bogged down and
throw around words like monad when talking about it.

I'd encourage you to look at your code and see if you have areas where
error handling code is detracting from the readability. This might be
an area where this, or something similar to it, would help.
