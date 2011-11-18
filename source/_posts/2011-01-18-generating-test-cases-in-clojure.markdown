---
comments: true
layout: post
title: Generating test cases in Clojure
categories: [clojure, code, testing]
---

Recently I was writing some data mining Clojure code which needed to parse a log file and do some transforms of the data. Some of the transforms were dependent on data found across multiple lines. There was no ordering or proximity guarantees to these lines.

This required the code to handle a variety of situations. After writing a couple simple tests and getting those passing I wanted to more extensively test my solution. I was lazy though and did not want to hand code all of the potential orderings.  Enter `permutations`.

`permutations` is a function out of [clojure.contrib.combinatorics](http://clojure.github.com/clojure-contrib/combinatorics-api.html). As the name suggests, you give it a collection and it returns a lazy sequence containing all the different permutations of the elements in that collection. An example is below.

``` clojure
    user>(ns generate)
    generate>(use '[clojure.contrib.combinatorics :only [permutations]])
    nil
    generate> (permutations [:a :b :c])
    ((:a :b :c) (:a :c :b) (:b :a :c) (:b :c :a) (:c :a :b) (:c :b :a))
```

You can already see where this is going. I was able to use `permutations` to generate all the potential different orderings of the input. This saved me the trouble of having to do that by hand.

One difficulty of generating test inputs pragmatically is telling what sort of inputs caused it to fail. To get around this I used the rarely used (at least in code I'm working on) second argument of [clojure.test's](http://clojure.github.com/clojure/clojure.test-api.html#clojure.test/is) `is`. This second argument is a message that prints on a failure.

Below is a contrived example of using `permutations` to test an obviously wrong `silly-add` function. `silly-add` is defined below.

``` clojure
    generate> (defn silly-add
                  [x & xs]
                  (if (zero? x)
                      (apply + 40 xs)
                      (apply + x xs)))
    #'generate/silly-add
```

Below is a test that uses `permutations` to exercise `silly-add` with all the potential orderings three input numbers. Note that it takes advantage of the second argument to `is`. Without this we would not know what input caused the failure.

``` clojure
    generate> (use 'clojure.test)
    nil
    generate> (deftest generate-some-tests
                (doseq [input (permutations [1 0 9])]
                       (is (= 10 (apply silly-add input))
                           (str "Failed on input: " (seq input)))))
    #'generate/generate-some-tests
```

Running the test we see that there is clearly an error.

``` clojure
    generate> (run-tests)
    Testing generate

    FAIL in (generate-some-tests) (NO_SOURCE_FILE:1)
    Failed on input: (0 1 9)
    expected: (= 10 (apply silly-add input))
      actual: (not (= 10 50))

    FAIL in (generate-some-tests) (NO_SOURCE_FILE:1)
    Failed on input: (0 9 1)
    expected: (= 10 (apply silly-add input))
      actual: (not (= 10 50))
```

`permutations` saved me a bit of time and let me test some situations that I otherwise would not have tested. This actually exposed a subtle bug in my code. Hopefully it can do the same for you.
