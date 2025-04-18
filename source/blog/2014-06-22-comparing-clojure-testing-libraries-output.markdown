---
dated-url: true
layout: post
title: "Comparing Clojure Testing Libraries: Output"
date: 2014-06-22 21:16
comments: true
published: true
description: An in-depth comparison of the output of Clojure testing libraries.
keywords: clojure, testing, clojure.test, midje, expectations, speclj
categories:
- clojure
- testing
---

I recently became interested in how Clojure testing libraries help you when there is a test failure.
This interest resulted in me [exploring](https://github.com/jakemcc/clojure-test-bed) different Clojure testing libraries.
I created the same tests using **clojure.test** (with and without [humane-test-output](https://github.com/pjstadig/humane-test-output)), [expectations](http://jayfields.com/expectations/), [Midje](https://github.com/marick/Midje), and [Speclj](http://speclj.com/) and looked at the output.

I ran all of these examples using Leiningen. **Midje**, **Speclj**, and **expectations** color their output but I'm not going to try to reproduce that here.
The color added by **Midje** and **expectations** is useful. **Speclj** color hurt its readability.
I use a dark colored terminal and **Speclj** colors the line that tells where the failure occurs black.
This made it hard to read.

I'm not going to show what the tests look like for each testing library past the first comparison.
How a test in expressed is important but not what I want to focus on in this post.

Comparing Strings
-----------------

Going to start off with a basic string comparison.
The failing test compares two strings that only differ by one character.

##### clojure.test #####

Most (hopefully all) Clojure programmers should be familiar with **clojure.test**.
It is the testing library that is included with Clojure.

``` clojure
(ns example.string-test
  (:require [clojure.test :refer :all]))

(deftest string-comparisons
  (is (= "strings equal" "strings equal"))
  (is (= "space" "spice")))
```

The output below is what you get when the above test runs.
Even in this simple example it isn't the easiest to read.
It doesn't make it easy to find the expected or actual values.

``` console clojure.test output
FAIL in (string-comparisons) (string_test.clj:6)
expected: (= "space" "spice")
  actual: (not (= "space" "spice"))
```

Below is the same test but with **humane-test-output** enabled.
It is easy to read the output and see the expected and actual value.
It even provides a diff between them although in this situation it isn't that useful.

``` console clojure.test with humane-test-output
FAIL in (string-comparisons) (string_test.clj:6)
expected: "space"
  actual: "spice"
    diff: - "space"
          + "spice"
```

##### expectations #####

Another testing library is Jay Field's [expectations](http://jayfields.com/expectations/).
You can see from the example that it has a fairly minimal syntax.

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

The output from **expectations** is very readable.
You can easily pick out the expected and actual values.
It also shows you where the string starts to diverge.

##### Speclj #####

Before writing this post I had zero experience with Micah Martin's [Speclj](http://speclj.com/).
Below is my translation of the failing string test and its output.

``` clojure
(ns example.string-spec
  (:require [speclj.core :refer :all]))

(describe "String comparisons"
  (it "have nice error message"
      (should= "space" "spice")))
```

``` console Speclj
9) String comparisons have nice error message
   Expected: "space"
        got: "spice" (using =)
   /Users/jake/src/jakemcc/example/spec/example/string_spec.clj:7
```

**Speclj**'s test output above is an improvement over **clojure.test**.
You can easily find the expected and actual values.
It doesn't provide any help with diagnosing how those values are different.

##### Midje #####

I have a little bit of experience with Brian Marick's [Midje](https://github.com/marick/Midje).
Unlike the other libraries it switches up the assertion syntax.
In **Midje** the expected value is on the right side of `=>`.

``` clojure
(ns example.string-test
  (:require [midje.sweet :refer :all]))

(fact "strings are equal"
  "string is equal" => "string is equal")

(fact "strings not equal"
   "spice" => "space")
```

``` console Midje
FAIL "strings not equal" at (string_test.clj:8)
    Expected: "space"
      Actual: "spice"
```

**Midje**'s output is similar to **Speclj**'s.
You can quickly find the expected and actual values but it doesn't help you spot the difference.

### String Comparison Winner ###

**expectations** wins for best output.
You can easily spot the expected and actual values and it also helps you find the difference between the strings.

The worst output comes from **clojure.test**.
It doesn't make it easy to spot the difference or even find the expected and actual values.

Comparing Maps
--------------

For maps I've setup three assertions.
The first has an extra key-value pair in the actual value.
The second has an extra in the expected value.
The final assertion has a different value for the `:cheese` key.
The **clojure.test** example is below.


``` clojure
(deftest map-comparisons
  (is (= {:sheep 1} {:cheese 1 :sheep 1}))
  (is (= {:sheep 1 :cheese 1} {:sheep 1}))
  (is (= {:sheep 1 :cheese 1} {:sheep 1 :cheese 5})))
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

Unsurprisingly the default **clojure.test** output for maps suffers from the same problems found in the string comparisons.
To find the actual and expected values you need to manually parse the output.

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

Above is the output of using **clojure.test** with **humane-test-output**.
It is a big improvement over the default **clojure.test**.
You can quickly see the expected and actual values.
Unlike with the string assertions the diff view is actually helpful.
The diffs do a good job of helping you identify the error.

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

**expectations** does a pretty good job helping you as well.
As before, you can clearly read the expected and actual values.
**expectations** also provides some hint as to what is different between the maps.
I find the English descriptions a bit easier to read than **humane-test-output**'s diff view.
Still seeing lines like line 7 (`in expected, not actual: null`) is a bit confusing and the output would be improved if it was suppressed.

I'm just going to lump **Speclj** and **Midje** together.
The output for each is below.
They both improve over **clojure.test** by making it easy to see the expected and actual value.
They both don't do anything beyond that.

``` console Speclj
4) map comparisons have nice error messages when extra entries keys present
   Expected: {:sheep 1}
        got: {:cheese 1, :sheep 1} (using =)
   /Users/jake/src/jakemcc/example/spec/example/map_spec.clj:7

5) map comparisons have nice error messages when missing an entry
   Expected: {:cheese 1, :sheep 1}
        got: {:sheep 1} (using =)
   /Users/jake/src/jakemcc/example/spec/example/map_spec.clj:9

6) map comparisons have nice error messages when mismatched values
   Expected: {:cheese 5, :sheep 1}
        got: {:cheese 1, :sheep 1} (using =)
   /Users/jake/src/jakemcc/example/spec/example/map_spec.clj:11
```

``` console Midje
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

### Map Comparison Winner ###

Tie between **humane-test-output** and **expectations**.
Both do a good job of helping the reader spot the difference.

Comparing Sets
--------------

Next up are sets.
Only two assertions for this section.
One with the actual value having an extra member and one test where it is missing a member.

``` clojure
(ns example.set-test
  (:require [clojure.test :refer :all]))

(deftest set-comparisons
  (is (= #{:a :b} #{:a :b :c}))
  (is (= #{:a :b :c} #{:a :b})))
```

First up is the basic **clojure.test** output.
It suffers from the same problem it has suffered this entire time.
It doesn't make it easy to read the expected and actual values.

``` console clojure.test
FAIL in (set-comparisons) (set_test.clj:5)
expected: (= #{:b :a} #{:c :b :a})
  actual: (not (= #{:b :a} #{:c :b :a}))

FAIL in (set-comparisons) (set_test.clj:6)
expected: (= #{:c :b :a} #{:b :a})
  actual: (not (= #{:c :b :a} #{:b :a}))
```

No surprises with **humane-test-output**.
It improves the **clojure.test** output by making it easy to read the expected and actual values.
The diff view also helps figure out what is causing the assertion to fail.

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

**expectations** once again delivers nice output.
It continues to be easy to find the expected and actual values and helps you spot the differences with a diff view.

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

**Speclj** and **Midje** both have better output than the basic **clojure.test**.

``` console Speclj
7) set comparisons have nice error messages when missing item
   Expected: #{:b :a}
        got: #{:c :b :a} (using =)
   /Users/jake/src/jakemcc/example/spec/example/set_spec.clj:9

8) set comparisons have nice error messages when more items
   Expected: #{:c :b :a}
        got: #{:b :a} (using =)
   /Users/jake/src/jakemcc/example/spec/example/set_spec.clj:11
```

``` console Midje
FAIL "set is superset of expected" at (set_test.clj:5)
    Expected: #{:a :b}
      Actual: #{:a :b :c}

FAIL "set is subset of expected" at (set_test.clj:8)
    Expected: #{:a :b :c}
      Actual: #{:a :b}
```

### Set Comparison Winner ###

Similar to the winner of the map comparisons I'm going to split the victory between **expectations** and **humane-test-output**.

Comparing Lists
---------------

Next up we compare lists (and lists to vectors).
There are three comparisons; one with an extra element, one with same length but a mismatched element, and one comparing a vector and list with drastically different contents.

``` clojure
(ns example.seq-test
  (:require [clojure.test :refer :all]))

(deftest list-comparisons
  (is (= '(1 2 3) '(1 2 3 4)))
  (is (= '(1 2 4) '(1 2 3)))
  (is (= '(9 8 7) [1 2 3])))
```

First up **clojure.test**.
Same issues as with all the previous comparisons.

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

Once again **humane-test-output** improves upon **clojure.test**.
Only interesting difference from previous comparisons is that the diff view ends up having `nil` values in it where the elements are the same.

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

**expectations** continues to have good output.
It tries to help you out as well.
You'll notice that it also has `nil` values inserted where the lists are the same.

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

Unsurprisingly, **Speclj** and **Midje** are better than **clojure.test** but again don't go beyond making easy to find the expected and actual values.

``` console Speclj
1) List/vector comparisons when there is an extra element
   Expected: (1 2 3)
        got: (1 2 3 4) (using =)
   /Users/jake/src/jakemcc/example/spec/example/string_spec.clj:7

2) List/vector comparisons when there is a mismatched element
   Expected: (1 2 4)
        got: (1 2 3) (using =)
   /Users/jake/src/jakemcc/example/spec/example/string_spec.clj:9

3) List/vector comparisons when comparing different types
   Expected: (9 8 7)
        got: [1 2 3] (using =)
   /Users/jake/src/jakemcc/example/spec/example/string_spec.clj:11
```

``` console Midje
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

### List Comparison Winner ###

I find the **clojure.test** with **humane-test-output** to be a bit easier to read than **expectations**.
Both have better output than the basic **clojure.test**, **Speclj**, and **Midje**.

## Overall Winner ##

If I were picking a testing library based entirely on what a failing test looks like I would use **expectations**.
My second pick would be **clojure.test** with **humane-test-output**.

It is great that Clojure ships with **clojure.test**.
It is unfortunate that it does so little to help you read a failing test.
Every library I tried has better output than **clojure.test**.

### Addendum ###

_Added 2014/06/23_

Colin Jones [points out](http://jakemccrary.com/blog/2014/06/22/comparing-clojure-testing-libraries-output/#comment-1449451549) that Speclj provides `should==`.
`should==` checks that the expected and actual value have the same contents.
He provided a [gist](https://gist.github.com/trptcolin/7e1ece5179581085730f) that shows the difference.
