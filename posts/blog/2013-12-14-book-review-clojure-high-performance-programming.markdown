---
layout: post
title: "Book Review: Clojure High Performance Programming"
date: 2013-12-14 16:39
comments: true
categories: [clojure, book-review]
---

I was recently approached by Packt Publishing asking if I'd review
Shantanu Kuma's book 
[Clojure High Performance Programming](http://www.packtpub.com/clojure-high-performance-programming/book)
. It sounded interesting so I took them up on
their offer for a free copy and read it over two flights.

Unsurprisingly the
[table of contents](http://www.packtpub.com/clojure-high-performance-programming/book#chapter_1)
does a good job describing the book. This book doesn't
dive too deep into any one topic but instead gives you a taste of
each.

Overall the book was pretty good. It provides interesting examples of
real world Clojure code that solve specific performance problems. It
talks about host performance, both JVM and hardware, concerns which are
both areas that shouldn't be overlooked. I thought the book was best
when showing examples of well performing code from libraries.

I'd recommend this book for developers who aren't past the beginning
stages of writing performant code. It does a good job introducing the
topics you'll want to think about when trying to craft well performing
programs.

It isn't for the developer who has spent years optimizing code for
performance. Those developers are already going to be familiar with
the language and concerns of writing high performance code.

If I could add anything to the book it would be a chapter about
measuring performance in production. If you are writing high
performance programs it has been my experience that you **must**
measure in production. This is easiest to do if you build measuring in
from the very beginning.
