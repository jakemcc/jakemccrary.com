---
layout: post
title: "Comparing Clojure testing library outputs"
date: 2014-06-21 10:51
comments: true
published: false
categories: [clojure, testing]
---

At an [Outpace](http://www.outpace.com/) off-site
[Jeff Langr](http://langrsoft.com/) took advantage of the
[open space](http://www.mindviewinc.com/Conferences/OpenSpaces) format
and lead a session where he showed off a Clojure project that uses
Midje as its testing library. It was a good session but I left
disappointed with how Midje reports test failures.

This inspired me to spend some time
[exploring](https://github.com/jakemcc/clojure-test-bed) different
Clojure testing libraries. The rest of this post comapares test output
from clojure.test (with and without humane-test-output), expectations,
Midje, and Speclj. I'm most familiar with clojure.test and
expectations so please leave a comment and open a pull request if the
clojure-test-bed project can be improved.

I'm not going go show what the tests look like for each testing
library past the first comparison. How a test in expressed is
important but not what I want to focus on in this post.

Comparing strings
-----------------

##### clojure.test #####

``` clojure
(ns example.string-test
  (:require [clojure.test :refer :all]))

(deftest string-comparisons
  (is (= "strings equal" "strings equal"))
  (is (= "space" "spice")))
```

``` console clojure.test output
FAIL in (string-comparisons) (string_test.clj:6)
expected: (= "space" "spice")
  actual: (not (= "space" "spice"))
```

The example above is going go be pretty familiar to most Clojure
authors. `clojure.test` is the testing library that comes with
Clojure. Below is an example where humane-test-output is enabled.

``` console clojure.test with humane-test-output
FAIL in (string-comparisons) (string_test.clj:6)
expected: "space"
  actual: "spice"
    diff: - "space"
          + "spice"
```

As you can see the output is a bit more humane. You no longer have to
parse s-expressions to pull out the expected and actual values. Though
not really useful here it also shows a diff.

##### expectations #####

Fewer programmers have probably used Jay Field's
[expectations](http://jayfields.com/expectations/). It is library with
a mimimal syntax. Below are the same tests as above written using expectations.


``` clojure
(ns example.string-expectations
  (:require [expectations :refer :all]))

(expect "strings equal" "strings equal")
(expect "space" "spice")
```

``` console expectations output
failure in (string_expectations.clj:5) : example.string-expectations
(expect "space" "spice")

           expected: "space"
                was: "spice"

           matches: "sp"
           diverges: "ace"
                  &: "ice"
```

The output from expectations is pretty nice. You can easily pick out
what the expected and actual values. It also shows you where the
string starts to diverge.

##### speclj #####

Before writing this post I had zero experiance with
[Speclj](http://speclj.com/). Unlike expectations it provides a
significant number of [functions](http://speclj.com/docs).

``` clojure
(ns example.string-spec
  (:require [speclj.core :refer :all]))

(describe "String comparisons"
 (it "have nice error message"
     (should= "space" "spice")))
```

``` console speclj
  9) String comparisons have nice error message
     Expected: "space"
          got: "spice" (using =)
     /Users/jake/src/jakemcc/example/spec/example/string_spec.clj:7
```

Speclj's test output above isn't hard to parse. Pretty easily can pick
out the expected and actual values. Definitly an improvement over the basic
`clojure.test` output. It be nice if it did a bit more to help you spot the difference.

##### Midje #####

[Midje](https://github.com/marick/Midje) is used by some of the
projects I work on so I have some experiance with it. Unlike the other
libraries mentioned it switches up the assertion syntax. In Midje the
expected value is on the right side of `=>`.

``` clojure
(ns example.string-test
  (:require [midje.sweet :refer :all]))

(fact "strings are equal"
  "string is equal" => "string is equal")

(fact "strings not equal"
   "spice" => "space")
```

``` console midje
FAIL "strings not equal" at (string_test.clj:8)
    Expected: "space"
      Actual: "spice"
```

Midje's failing string comparison output is similar to Speclj's. You
can quickly find the expected and actual values but it doesn't do
anything to help you spot the difference.

##### Best at string comparisons? #####

Based on this sample my vote on best output for failing string
comparisons goes to expectations. You can easily spot the expected and
actual values and it also helps you find the difference between the
strings.

The worst output comes from clojure.test. It doesn't make it easy to
spot the difference or even find the expected and actual values.

Comparing maps
--------------

#### clojure.test ####

``` clojure
(ns example.map-test
  (:require [clojure.test :refer :all]))

(deftest map-comparisons
  (is (= {:sheep 1} {:goats 1 :sheep 1}))
  (is (= {:goats 1 :sheep 1} {:sheep 1})))
```

``` console clojure.test
FAIL in (map-comparisons) (map_test.clj:5)
expected: (= {:sheep 1} {:cheese 1, :sheep 1})
  actual: (not (= {:sheep 1} {:cheese 1, :sheep 1}))

FAIL in (map-comparisons) (map_test.clj:6)
expected: (= {:sheep 1, :cheese 1} {:sheep 1})
  actual: (not (= {:cheese 1, :sheep 1} {:sheep 1}))

FAIL in (map-comparisons) (map_test.clj:7)
expected: (= {:sheep 1, :cheese 1} {:sheep 1, :cheese 5})
  actual: (not (= {:cheese 1, :sheep 1} {:cheese 5, :sheep 1}))
```

``` console clojure.test with humane-test-output
FAIL in (map-comparisons) (map_test.clj:5)
expected: {:sheep 1}
  actual: {:cheese 1, :sheep 1}
    diff: + {:cheese 1}

FAIL in (map-comparisons) (map_test.clj:6)
expected: {:cheese 1, :sheep 1}
  actual: {:sheep 1}
    diff: - {:cheese 1}

FAIL in (map-comparisons) (map_test.clj:7)
expected: {:cheese 1, :sheep 1}
  actual: {:cheese 5, :sheep 1}
    diff: - {:cheese 1}
          + {:cheese 5}
```

``` console expectations
failure in (map_expectations.clj:6) : example.map-expectations
(expect {:sheep 1} {:sheep 1, :cheese 1})

           expected: {:sheep 1}
                was: {:cheese 1, :sheep 1}

           in expected, not actual: null
           in actual, not expected: {:cheese 1}

failure in (map_expectations.clj:7) : example.map-expectations
(expect {:sheep 1, :cheese 1} {:sheep 1})

           expected: {:cheese 1, :sheep 1}
                was: {:sheep 1}

           in expected, not actual: {:cheese 1}
           in actual, not expected: null

failure in (map_expectations.clj:8) : example.map-expectations
(expect {:sheep 1, :cheese 5} {:sheep 1, :cheese 1})

           expected: {:cheese 5, :sheep 1}
                was: {:cheese 1, :sheep 1}

           in expected, not actual: {:cheese 5}
           in actual, not expected: {:cheese 1}
```

``` console speclj
  4) map comparsions have nice error messages when extra entries keys present
     Expected: {:sheep 1}
          got: {:cheese 1, :sheep 1} (using =)
     /Users/jake/src/jakemcc/example/spec/example/map_spec.clj:7

  5) map comparsions have nice error messages when missing an entry
     Expected: {:cheese 1, :sheep 1}
          got: {:sheep 1} (using =)
     /Users/jake/src/jakemcc/example/spec/example/map_spec.clj:9

  6) map comparsions have nice error messages when mismatched values
     Expected: {:cheese 5, :sheep 1}
          got: {:cheese 1, :sheep 1} (using =)
     /Users/jake/src/jakemcc/example/spec/example/map_spec.clj:11
```

``` console midje
FAIL "map is missing an entry" at (map_test.clj:5)
    Expected: {:cheese 1, :sheep 1}
      Actual: {:sheep 1}

FAIL "map has an extra entry" at (map_test.clj:8)
    Expected: {:sheep 1}
      Actual: {:cheese 1, :sheep 1}

FAIL "map has a different value" at (map_test.clj:11)
    Expected: {:cheese 5, :sheep 1}
      Actual: {:cheese 1, :sheep 1}
```

Comparing sets
--------------


``` clojure
(ns example.set-test
  (:require [clojure.test :refer :all]))

(deftest set-comparisons
  (is (= #{:a :b} #{:a :b :c}))
  (is (= #{:a :b :c} #{:a :b})))
```

``` console clojure.test
FAIL in (set-comparisons) (set_test.clj:5)
expected: (= #{:b :a} #{:c :b :a})
  actual: (not (= #{:b :a} #{:c :b :a}))

FAIL in (set-comparisons) (set_test.clj:6)
expected: (= #{:c :b :a} #{:b :a})
  actual: (not (= #{:c :b :a} #{:b :a}))
```

``` console clojure.test with humane-test-output
FAIL in (set-comparisons) (set_test.clj:5)
expected: #{:b :a}
  actual: #{:c :b :a}
    diff: + #{:c}

FAIL in (set-comparisons) (set_test.clj:6)
expected: #{:c :b :a}
  actual: #{:b :a}
    diff: - #{:c}
```

``` console expectations
failure in (set_expectations.clj:4) : example.set-expectations
(expect #{:b :a} #{:c :b :a})

           expected: #{:b :a}
                was: #{:c :b :a}

           in expected, not actual: null
           in actual, not expected: #{:c}

failure in (set_expectations.clj:5) : example.set-expectations
(expect #{:c :b :a} #{:b :a})

           expected: #{:c :b :a}
                was: #{:b :a}

           in expected, not actual: #{:c}
           in actual, not expected: null
```

``` console speclj
  7) set comparisons have nice error messages when missing item
     Expected: #{:b :a}
          got: #{:c :b :a} (using =)
     /Users/jake/src/jakemcc/example/spec/example/set_spec.clj:9

  8) set comparisons have nice error messages when more items
     Expected: #{:c :b :a}
          got: #{:b :a} (using =)
     /Users/jake/src/jakemcc/example/spec/example/set_spec.clj:11
```

``` console midje
FAIL "set is superset of expected" at (set_test.clj:5)
    Expected: #{:a :b}
      Actual: #{:a :b :c}

FAIL "set is subset of expected" at (set_test.clj:8)
    Expected: #{:a :b :c}
      Actual: #{:a :b}
```

Comparing lists
---------------

``` clojure
(ns example.seq-test
  (:require [clojure.test :refer :all]))

(deftest list-comparisons
  (is (= '(1 2 3) '(1 2 3 4)))
  (is (= '(1 2 4) '(1 2 3)))
  (is (= '(1 2 4) [1 2 4])))
```

``` console clojure.test
FAIL in (list-comparisons) (seq_test.clj:5)
expected: (= (quote (1 2 3)) (quote (1 2 3 4)))
  actual: (not (= (1 2 3) (1 2 3 4)))

FAIL in (list-comparisons) (seq_test.clj:6)
expected: (= (quote (1 2 4)) (quote (1 2 3)))
  actual: (not (= (1 2 4) (1 2 3)))

FAIL in (list-comparisons) (seq_test.clj:7)
expected: (= (quote (9 8 7)) [1 2 3])
  actual: (not (= (9 8 7) [1 2 3]))
```

``` console clojure.test with humane-test-output
FAIL in (list-comparisons) (seq_test.clj:5)
expected: (1 2 3)
  actual: (1 2 3 4)
    diff: + [nil nil nil 4]

FAIL in (list-comparisons) (seq_test.clj:6)
expected: (1 2 4)
  actual: (1 2 3)
    diff: - [nil nil 4]
          + [nil nil 3]

FAIL in (list-comparisons) (seq_test.clj:7)
expected: (9 8 7)
  actual: [1 2 3]
    diff: - [9 8 7]
          + [1 2 3]
```

``` console expectations
failure in (list_expectations.clj:4) : example.list-expectations
(expect '(1 2 3) '(1 2 3 4))

           expected: (1 2 3)
                was: (1 2 3 4)

           in expected, not actual: null
           in actual, not expected: [nil nil nil 4]
           actual is larger than expected

failure in (list_expectations.clj:5) : example.list-expectations
(expect '(1 2 4) '(1 2 3))

           expected: (1 2 4)
                was: (1 2 3)

           in expected, not actual: [nil nil 4]
           in actual, not expected: [nil nil 3]

failure in (list_expectations.clj:6) : example.list-expectations
(expect '(9 8 7) [1 2 3])

           expected: (9 8 7)
                was: [1 2 3]

           in expected, not actual: [9 8 7]
           in actual, not expected: [1 2 3]
```

``` console speclj
  1) List/vector comparissons when there is an extra element
     Expected: (1 2 3)
          got: (1 2 3 4) (using =)
     /Users/jake/src/jakemcc/example/spec/example/string_spec.clj:7

  2) List/vector comparissons when there is a mismatched element
     Expected: (1 2 4)
          got: (1 2 3) (using =)
     /Users/jake/src/jakemcc/example/spec/example/string_spec.clj:9

  3) List/vector comparissons when comparing different types
     Expected: (9 8 7)
          got: [1 2 3] (using =)
     /Users/jake/src/jakemcc/example/spec/example/string_spec.clj:11
```

``` console midje
FAIL "lists are different sizes" at (seq_test.clj:5)
    Expected: (1 2 3)
      Actual: (1 2 3 4)

FAIL "lists have different entries" at (seq_test.clj:8)
    Expected: (1 2 4)
      Actual: (1 2 3)

FAIL "compare very different list like values" at (seq_test.clj:14)
    Expected: (9 8 7)
      Actual: [1 2 3]
```

speclj test runner prints file and line number in black. Seems like a
poor default when having a dark terminal is fairly common.
