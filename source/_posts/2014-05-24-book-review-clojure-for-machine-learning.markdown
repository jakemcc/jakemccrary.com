---
layout: post
title: "Book Review: Clojure for Machine Learning"
date: 2014-05-24 10:09
comments: true
categories: [clojure, book-review]
---

I was recently given a review copy of
[Clojure for Machine Learning](http://www.packtpub.com/clojure-for-machine-learning/book).
I have an academic familiarity with machine learning techniques and
presented on a few at [speakerconf](http://speakerconf.com/)
2012. I haven't explored machine learning in Clojure since preparing
that talk and was excited to read a book on the topic.

The book gives a shallow introduction to many
[different topics](http://www.packtpub.com/clojure-for-machine-learning/book#chapter_0).
It does so through a bit of mathematics and much more code. Depending
on the section, the code examples implement the algorithm being
discussed, show you how to use a specific library, or do both.

An aspect I particularly enjoy about the code examples is that
they always start by showing what dependencies should be added to
your `project.clj` file. This is done even if the library has been used in
a previous chapter. Because of this every example can stand on its own.

Something that can almost always be improved about Clojure
 examples is that namespaces should be referenced using the
`require` form with a namespace alias. Even if that require requires a namespace
with a terrible alias, such as `(require '[example :as e])`, it makes
the example code easier to understand. Being able to read `e/a-func`
instead of `a-func` makes it more explicit as to where that function
is located and aides understanding. 

I rate all
[my books](https://www.goodreads.com/user/show/3431614-jake-mccrary)
by the [goodreads](http://goodreads.com/) five star scale[^1]. This
book earns three stars. Even with my limited machine learning
background I didn't learn anything new but I was introduced to some
Clojure libraries and enjoyed seeing Clojure implementations of
machine learning techniques.

If you enjoy Clojure and the
[table of contents](http://www.packtpub.com/clojure-for-machine-learning/book#chapter_0)
excites you then you'll most likely find this book interesting. If you
want to maximize your learning I'd recommend taking an
[online course](https://www.coursera.org/courses?search=machine%20learning)
in machine learning[^2]. It will be a larger time investment but you'll
leave with a deeper understanding.


[^1]: 1 star = did not like, 2 stars = it was ok, 3 stars = liked it, 4 stars = really liked it, 5 stars = loved it.
[^2]: I took the original offering from [Stanford](https://www.coursera.org/course/ml) when it was first offered. Post about it [here](http://jakemccrary.com/blog/2011/12/29/reflections-on-stanfords-online-class-experiment/).
