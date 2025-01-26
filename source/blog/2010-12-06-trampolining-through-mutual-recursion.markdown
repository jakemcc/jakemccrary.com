---
dated-url: true
layout: post
comments: true
title: Trampolining through mutual recursion with Clojure
categories: [clojure, code]
date: "2010-12-06"
---




The other day I stumbled across some [Clojure](http://clojure.org) code that used [mutual recursion](http://en.wikipedia.org/wiki/Mutual_recursion). Mutual recursion can be a valuable tool when solving a problem. Unfortunately because of the lack of [tail call optimization](http://en.wikipedia.org/wiki/Tail_call) on the JVM this can be a dangerous technique when writing Clojure code. It can be easy to forget about this limitation and end up writing code that blows the stack.

Take the classic even/odd checking code from the [Wikipedia](http://en.wikipedia.org/wiki/Mutual_recursion) page. If we just translate it to Clojure it will cause a [stack overflow](http://en.wikipedia.org/wiki/Stack_overflow) error when we pass in a large number. The massive number of function calls require before returning causes too much memory to be consumed.

``` clojure
(declare my-odd?)

(defn my-even? [n]
  (if (zero? n)
    true
    (my-odd? (dec (Math/abs n)))))

(defn my-odd? [n]
  (if (zero? n)
    false
    (my-even? (dec (Math/abs n)))))

user> (my-even? 1000000)
; Evaluation aborted. <- this is a result of java.util.StackOverflowError
```

Luckily since Clojure 1.0 there has been a useful function for dealing with this. `trampoline`, with minor modifications to your code, can be used to get around the lack of tail call optimizations ([docs here](http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/trampoline)).

`trampoline` takes a function (and, if needed, arguments to pass into the function) and calls it. If the function returns a function then `trampoline` calls that. As long as functions are returned `trampoline` will continue calling them. When a non-function value is returned `trampoline` returns, passing through the value.

To make our sample code work with `trampoline` we simply change our functions to return a closure which wraps the call that was previously being executed. This just entails putting a `#` before the final s-exp. This takes advantage of Clojure's anonymous function syntax to change the function call into a closure which is returned.

``` clojure
(defn my-even? [n]
  (if (zero? n)
    true
    #(my-odd? (dec (Math/abs n)))))

(defn my-odd? [n]
  (if (zero? n)
    false
    #(my-even? (dec (Math/abs n)))))
```

By doing this we've changed how the caller interacts with `my-even?` and `my-odd?`. It now needs to be called by `trampoline`.

``` clojure
user> (trampoline my-even? 1000000)
true
```

Now we no longer suffer from the stack overflow error.

I think we can still do better though, because now the caller of `my-even?` and `my-odd?` suffers since they are forced to remember to use `trampoline`. By forcing this on the caller, we've pushed what should be hidden implementations details into the callers code. We can fix this by pushing the use of `trampoline` into our functions.

``` clojure
(defn my-even? [n]
  (letfn [(e? [n]
              (if (zero? n)
                true
                #(o? (dec (Math/abs n)))))
          (o? [n]
              (if (zero? n)
                false
                #(e? (dec (Math/abs n)))))]
    (trampoline e? n)))

(defn my-odd? [n]
  (not (my-even? n)))

user> (my-even? 1000000)
true
user> (my-odd? 1000000)
false
```

Now we have the best of both worlds. Mutual recursion without the worry of a stack overflow and functions that don't force the caller to be aware of the implementation details.