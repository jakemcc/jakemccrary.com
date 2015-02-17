---
layout: post
title: "Releasing the Functional JavaScript Companion"
date: 2013-07-09 20:22
comments: true
categories: [kindle, dictionary]
---

You may have seen me [tweeting](https://twitter.com/jakemcc/status/352893242473328641) [about](https://twitter.com/jakemcc/status/349709102986969088) building custom Kindle dictionaries. A few months ago I made a
[custom Kindle dictionary](http://gum.co/dune-dictionary) for [Dune](http://www.amazon.com/gp/product/B00B7NPRY8/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B00B7NPRY8&linkCode=as2&tag=jakemccrary08-20&linkId=LC2NFEXWA7JXW57B) and my reading experience greatly improved. Being able to look up unknown terms as easily as English words was amazing. Ever since I've been looking for other books that could benefit from having a custom dictionary. While reading Fogus's [Functional JavaScript](http://www.amazon.com/gp/product/B00D624AQO/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B00D624AQO&linkCode=as2&tag=jakemccrary08-20&linkId=CUEYRNJIQSFNKUSM) I saw the opportunity to make a another.

I was taking my time reading Fogus's book and, as a result, found myself forgetting the implementation of functions defined earlier in the book. I wanted to be able to look up implementations easily and realized that a dictionary of function names to implementations would solve my problem.

I found the book's [repo](https://github.com/funjs/book-source) and confirmed the license would allow this. Then extracted the data (wrote a simple parser in Clojure, extracts functions that follow [this](https://github.com/funjs/book-source/blob/dc6c2a97cb1099654f3179fda0794b188fc26f11/chapter03.js#L117) format) and made a dictionary.

Steps to using my custom dictionary:

1. [Download](http://db.tt/eJzkIVfS) the dictionary (titled _Functional JavaScript Companion_).
2. Put it on your e-ink Kindle (transfer over USB or email it).
3. Change your default English dictionary to _Functional JavaScript Companion_.
4. Start reading _Functional JavaScript_. Look up function implementations the same way you would normal English words.

You can change your Kindle's default dictionary by navigating to `Settings > Device Options > Language and Dictionaries`. You don't need to do this with all custom dictionaries but it is pretty much a requirement for this one. Many of the function names are English words and as a result if you don't change the default to _Functional JavaScript Companion_ you'll end up looking up the definitions of standard English words.

This dictionary isn't perfect but it did improve my reading experience. One example of where it fails is if you look up the function `partial1` it will look up `partial`. This is result of how the Kindle looks up words. Another minor issue is that the functions are often too large to fit in the pop-up window. The fix to both of these is to click the "Show Full Definition" button of the pop-up to be taken to the dictionary. Another issue is that the numerous functions defined by composition (example: [`isOdd`](https://github.com/funjs/book-source/blob/dc6c2a97cb1099654f3179fda0794b188fc26f11/chapter03.js#L125)) are not parsed by my parser and therefor not part of the dictionary.

This was definitely a larger challenge than creating my custom [Dune dictionary](http://gum.co/dune-dictionary). It forced me to dive into the Amazon documentation a bit and figure out more of the markup language. I have notes on my experience creating Kindle dictionaries and sometime in the future will be writing a post with details about what I've learned.

I can't recommend Fogus's [Functional JavaScript](http://www.amazon.com/gp/product/B00D624AQO/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B00D624AQO&linkCode=as2&tag=jakemccrary08-20&linkId=CUEYRNJIQSFNKUSM) enough. If you do read it give my [dictionary](http://db.tt/eJzkIVfS) a shot. I think it makes the reading experience a bit nicer.






