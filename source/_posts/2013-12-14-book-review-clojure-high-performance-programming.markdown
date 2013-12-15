---
layout: post
title: "Book Review: Clojure High Performance Programming"
date: 2013-12-14 16:39
comments: true
categories: [clojure, book-review]
---

I was recently approached by Packt Publishing asking if I'd take the
time to review
[Clojure High Performance Programming](http://www.packtpub.com/clojure-high-performance-programming/book)
by Shantanu Kumar. The book sounded interesting so I took them up on
their offer for a free copy and read it while flying to attend a
wedding.

The
[table of contents](http://www.packtpub.com/clojure-high-performance-programming/book#chapter_1)
does a pretty good job of describing what each chapter is about so I'm
not going to do a chapter by chapter review. Take a look at the table
of contents and you'll have a pretty good idea of what this book
covers.

The book starts with an introduction to basic concerns when talking
about performance. It is a pretty decent introduction to performance
concerns and vocabulary. It does a good job of showing the reader that
it isn't just algorithm speed that influences performance.

Overall the book was pretty good. It does a good job giving a fairly
high level summary of performance concerns. It gives you some
interesting examples of real world Clojure code that solve specific
performance problems. It talks about how host performance affects your
code and how to benchmark. I enjoyed the book the most when it gave
examples of how certain libraries solved some performance problem.

I'd recommend this book for developers who aren't past the beginning
stages of writing performant code. It isn't for the developer who has
spent years optimizing code for performance. Those developers are
already going to be familiar with the language and concerns of writing
high performance code.

If I could add anything to the book it would be a chapter about
measuring performance in production. If you are writing high
performance programs it has been my experience that you **must**
measure in production. This is easiest to do if you build measuring in
from the very beginning.
