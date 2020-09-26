---
layout: post
title: "Creating a custom Kindle dictionary"
date: 2020-09-25 21:35:05 -0500
comments: true
published: false
description: PUT SUMMARY HERE 
keywords: 'csv, keywords, here'
categories: 
---

https://gumroad.com/l/dune-dictionary

Back in April 2013 I created and published a [custom Kindle dictionary](https://gumroad.com/l/dune-dictionary) for the book [Dune](http://www.amazon.com/gp/product/B00B7NPRY8/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B00B7NPRY8&linkCode=as2&tag=jakemccrary08-20&linkId=LC2NFEXWA7JXW57B).
I was reading Dune for my first time and there were plenty of unfamiliar words.
Since I was on my Kindle, I was very used to being able to select a word and see its meaning.

This didn't work for many of the words I attempted to look up.
This is because these words were unique to the Dune universe.
They existed in the glossary in the back of the book but flipping to that on a Kindle was a huge pain.

I printed out a term list from the Internet and carried that with me.
This was better but it was still annoying.

I was annoyed enough that I took a break from reading to dig into how to create a custom Kindle dictionary.
There wasn't a ton of great information online about how to do this.

Eventually, I turned to Amazon's [Kindle Publishing Guidelines](https://s3.amazonaws.com/kindlegen/AmazonKindlePublishingGuidelines.pdf) and, with some experimentation, figured out something that worked.
The link in the previous sentence is to the **current** documentation, which is much nicer than the [mid-2013 documentation](https://web.archive.org/web/20130408183149/http://s3.amazonaws.com/kindlegen/AmazonKindlePublishingGuidelines.pdf).
Using this documentation, I developed some Clojure code to generate my [dictionary](https://gumroad.com/l/dune-dictionary).

As far as I can tell, [my Dune dictionary](https://gumroad.com/l/dune-dictionary) was the very first custom Kindle dictionary for a fiction book.

## Making a dictionary

The steps to build a dictionary are minimal.

1. Construct your list of words and definitions.
1. Convert the list into an xhtml format specified by Amazon.
1. Construct a cover page.
1. Combine them together using `kindlegen`.
1. TEST IT AND MAKE THIS HEADER MATCH

### 1. Construct your list of words and definitions

There really isn't set instructions for this.
Source your words and definitions and store them in some format you'll be able to manipulate in a programming language.

### 2. Convert the list into an xhtml format specified by Amazon

Here I cheat a bit as it turns out you don't actually have to follow everything Amazon specifies.

Below is the skeleton of what you'll need to fill in.

```html
<html>
  <head>
    <link href="style.css" rel="stylesheet" type="text/css">
    <meta content="text/html" http-equiv="content-type">
  </head>
  <body>
    <dl>
      [PUT THE WORDS HERE]
    </dl>
  </body>
</html>
```

The `[PUT THE WORDS HERE]` part gets filled in with the markup for all of your words.
The basic structure of a word's entry looks like the following.

```html
<div class="definition">
  <idx:entry name="default">
    <dt><idx:orth>Aba</idx:orth></dt>
    <dd>A loose, usually black robe worn by Fremen women and Bene Gesserit sisters.</dd>
  </idx:entry>
</div>
<hr>
```

Each word's markup consists of a `div` block followed by a `hr`.
The `div` is given a class of `definition` and contains an `idx:entry`.

The `name` attribute on the `idx:entry` element defines what lookup index the entry is associated with.
Unless you are building a dictionary with multiple indexes, you can pretty much ignore it.
I've always set my to have `name=default`.

The markup you use inside the `idx:entry` element is mostly up to you.
I use the html tags `dt` and `dd` to surround the word and the definition.

Inside of the `dt` element you put a `idx:orth` element with its content being the word being looked up.

You can do some fancier stuff here as well.
This includes specifying if an exact match is required, alternative words that should result in the same lookup, and varying the search word from the displayed word.
Most of that isn't important for many dictionaries.

### 3. Construct a cover page.

WHAT ABOUT THE IMAGE? WHERE IS STYLE.CSS?

This is just a requirement of a Kindle.
Create an html file called `cover.html` and substitute in the appropiate values.

```html
<html>
  <head>
    <link href="style.css" rel="stylesheet" type="text/css">
    <meta content="text/html" http-equiv="content-type">
  </head>
  <body>
    <h1>Dune Dictionary</h1>
    <h3>Created by Jake McCrary</h3>
  </body>
</html>
```

### 4. Combine them together using `kindlegen`.

Download `kindlegen` from Amazon and use it to combine your files into a dictionary.

```bash
put that here
```

If your dictionary has a lot of words then you might run into problems having them all in a single file.
If that happens, you might be able to resolve it by splitting them into multiple xhmtl files.

### 5. Test your dictionary

Load the dictionary onto your Kindle.
You can do this by either using a USB cable or by emailing it to your Kindle's email address.

Once it is on your Kindle, you can set it to be the default dictionary.
Now when you try to look up a word, your dictionary is searched first.
This is a good time to confirm your formatting is good and the dictionary allows you to search for words.

## End

It was interesting writing this up so long after I originally wrote the code to generate my Dune dictionary.
Revisiting the Kindle Publishing documentation and there is a large difference between the mid-2013 document, the mid-2020 document, and what markup I generated.

I didn't add any of the markup that the mid-2020 guide claims is required.
There are also more features that are supported with the new format that either weren't there or were not documented seven years ago.

Some of those features, like requiring an exact match, might have been useful for my second dictionary, The Functional Javascript Companion.
If I were to make a new dictionary today, I'd need to experiment with some of those new features.

Regardless of the differences between the documentation and my actual implementation, my Dune dictionary works and I'm thankful it still does.

If you're ever in a situation where you think a custom dictionary would be useful, feel free to reach out.
I haven't shared any code here as it is pretty minimal and a worthwhile exercise to knock out in whatever language you can use to generate html.

Go forth and make dictionaries.

