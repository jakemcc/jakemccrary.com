---
dated-url: true
layout: post
title: "Book Review: Clojure Data Analysis Cookbook"
date: 2013-05-06 07:00
comments: true
categories: [clojure, book-review]
---

I spent the last week reading [^1] the [Clojure Data Analysis Cookbook](http://www.packtpub.com/clojure-data-analysis-cookbook/book) by Eric Rochester.
As you may expect from the name, this book follows a traditional cookbook format.
Each section presents a goal and then some code which achieves the goal.

The text covers a variety of data analysis topics.
Some include reading data from files, machine learning, graphing, and interfacing with other analysis tools.
I particularly enjoyed the section on lazily processing large data sets.
I find this is an area of frustration for many and this should serve as a reference to point them towards.

The examples are fairly easy to follow.
Many of the examples use `require` to alias dependent namespaces.
I think this is key when presenting Clojure examples.
Having to prefix calls to library functions causes them to stand out from uses of core Clojure functions.
It also lets readers know from which library each function comes from.
I would have liked to see all of the examples use `require` instead of `use` for pulling in dependencies because of the clarity it brings.

I do have a sort of nit-picky negative about this (in particular, the PDF I received from the Packt Publishing website) book.
While the vast majority of the code examples were well formatted every once in a while one would be poorly formatted.
Poorly formatted code in a book all about showing code is disappointing and interrupts the flow of reading a recipe.
One example of this is found in the first step of chapter 3's "Combining agents and STM" recipe.

#### Recommendation ####

Would I recommend getting this book?
If any section in the table of contents sounds useful to you then yes, you should buy the book.
It will be a useful reference.

Would I recommend reading this book front to back?
Probably not.
I would recommend reading sections that interest you and skimming others.

Just like a food cookbook's purpose (usually) isn't to teach you how to cook, this book will not teach you how to write Clojure.
It will help you become better at specific tasks.


[^1]: I was given this book to review by Packt Publishing. If you think you have something interesting to read and want another set of eyes on it, feel free to reach out. Depending on the topic I'm willing to give feedback before publication or potentially write a review after.
