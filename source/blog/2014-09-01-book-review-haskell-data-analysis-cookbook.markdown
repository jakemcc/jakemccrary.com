---
dated-url: true
layout: post
title: ! 'Book Review: Haskell Data Analysis Cookbook'
date: 2014-09-01 13:49
comments: true
published: true
categories:
- haskell
- book-review
---

Packt Publishing recently asked me to write a review of the book
[Haskell Data Analysis Cookbook](http://bit.ly/X0YQaL) by Nishant
Shukla. The book is broken into small sections that show you how to do
a particular task related to data analysis. These tasks vary from
reading a csv file or parsing json to listening to a stream of tweets.

I'm not a Haskell programmer. My Haskell experience is limited to
reading some books
([Learn You a Haskell for Great Good](http://learnyouahaskell.com/)
and most of [Real World Haskell](http://realworldhaskell.org/)) and
solving some toy problems. All of reading and programming happened
years ago though so I'm out of practice.

This book is not for a programmer that is unfamiliar with Haskell. If
you've never studied it before you'll find yourself turning towards
documentation. If you enter this book with a solid understanding of
functional programming you can get by with a smaller understanding of
Haskell but you will not get much from the book.

I've only read a few cookbook style books and this one followed the
usual format. It will be more useful as a quick reference than as
something you would read through. It doesn't dive deep into any topic
but does point you toward libraries for various tasks and shows a
short example of using them.

A common critic I have of most code examples applies to this book.
Most examples do not do qualified imports of namespaces or selective
imports of functions from namespaces. This is especially useful when
your examples might be read by people who are not be familiar with the
languages standard libraries. Reading code and immediately knowing
where a function comes from is incredibly useful to understanding.

The code for this book is available on
[GitHub](https://github.com/BinRoot/Haskell-Data-Analysis-Cookbook).
It is useful to look at the full example for a section. The examples
in the book are broken into parts with English explanations and I
found that made it hard to fully understand how the code fit together.
Looking at the examples in the GitHub repo helped.

#### Recommendation ####

I'd recommend this book for Haskell programmers who find the table of
contents interesting. If you read the table of contents and think it
would be useful to have a shallow introduction to the topics listed
then you'll find this book useful. It doesn't give a detailed dive
into anything but at least gives you a starting point.

If you either learning Haskell or using Haskell then this book doesn't
have much to offer you.