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

## NOTES

- Must have a primary index of words in alaphabtetical order
- Marked as dictionary, input and output languages must be dfeined properly
- Should have
  - A cover image
  - A copyright page
  - Any relevant front or back matter
  - Definitions of words
- opf file
  - ISO 639-1 language code for languages
  - defautl lookup index if more than one index

`kindlegen.exe [filename.opf] -c2 –verbose -dont_append_source`

## CONTENT

Back in April 2013 I created and published a [custom Kindle dictionary](https://gumroad.com/l/dune-dictionary) for the book [Dune](http://www.amazon.com/gp/product/B00B7NPRY8/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B00B7NPRY8&linkCode=as2&tag=jakemccrary08-20&linkId=LC2NFEXWA7JXW57B).
As far as I can tell, [my Dune dictionary](https://gumroad.com/l/dune-dictionary) was the very first custom Kindle dictionary for a fiction book.

I created it because I had started reading Dune for the first time and there were many of unfamiliar words.
At this point in my life, I was very used to being able to highlight a word on my Kindle and see its meaning.

This didn't work for many of the words in Dune because many of those words were unique to the Dune universe.
They existed in the included glossary but flipping to that on a Kindle was a huge pain.

I initially fixed this by printing a list for Wikipedia and carrying that with me.
This was better but it was still annoying.

I was so annoyed that I took a break from reading to dig into how to create a custom Kindle dictionary.
At the time, there wasn't a ton of great information online about how to do this.

Eventually, I found to Amazon's [Kindle Publishing Guidelines](https://s3.amazonaws.com/kindlegen/AmazonKindlePublishingGuidelines.pdf) and, with some experimentation, figured out something that worked.
The link in the previous sentence is to the **current** documentation, which is much nicer than the [mid-2013 documentation](https://web.archive.org/web/20130408183149/http://s3.amazonaws.com/kindlegen/AmazonKindlePublishingGuidelines.pdf).
The earlier documentation left me with questions and required some experimentation.

Using the mid-2013 documentation, I developed some Clojure code to generate my [dictionary](https://gumroad.com/l/dune-dictionary).
Doing this in 2013 was annoying.
The documentation was not good.

I recently read [Greg Egan's Diaspora](https://www.gregegan.net/DIASPORA/DIASPORA.html) and realized while reading it that I wanted a dictionary of the glossary.
I decided to take Diaspora's glossary and package it up as a dictionary for my own usage.
I could have just stuck with my 2013 generator but I decided to update it and write this article about creating a Kindle dictionary in late 2020.

The documentation is a bit better but it still isn't great.

https://web.archive.org/web/20200130040547mp_/http://kindlegen.s3.amazonaws.com/KindleGen_Mac_i386_v2_9.zip
https://web.archive.org/web/20200130040547mp_/http://kindlegen.s3.amazonaws.com/kindlegen_linux_2.6_i386_v2_9.tar.gz
https://web.archive.org/web/20200130040547mp_/http://kindlegen.s3.amazonaws.com/kindlegen_win32_v2_9.zip

## Making a dictionary

The steps to build a dictionary are minimal.

1. Construct your list of words and definitions.
1. Convert the list into an xhtml format specified by Amazon.
1. Construct a cover page. (sort of optional)
1. Construct a few other pages
1. Create a `.opf` file that ties everything together.
1. Combine them together using `kindlegen`.
1. TEST IT AND MAKE THIS HEADER MATCH

### 1. Construct your list of words and definitions

There really isn't set instructions for this.
Source your words and definitions and store them in some format you'll be able to manipulate in a programming language.

I've done this in a few different ways.
I've extracted a list of words from [source cord](/blog/2013/07/09/releasing-the-functional-javascript-companion/).
I've used Wikipedia and I've also grabbed words from glossary in a book.

### 2. Convert the list into an xhtml format specified by Amazon


Below is the basic skeleton of what Amazon wants you to have along with some inline styles.
This isn't all specified by Amazon nor is everything they specify actually required.
Below is what worked well for me.

```html
<html xmlns:math="http://exslt.org/math" xmlns:svg="http://www.w3.org/2000/svg"
      xmlns:tl="https://kindlegen.s3.amazonaws.com/AmazonKindlePublishingGuidelines.pdf"
      xmlns:saxon="http://saxon.sf.net/" xmlns:xs="http://www.w3.org/2001/XMLSchema"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns:cx="https://kindlegen.s3.amazonaws.com/AmazonKindlePublishingGuidelines.pdf"
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:mbp="https://kindlegen.s3.amazonaws.com/AmazonKindlePublishingGuidelines.pdf"
      xmlns:mmc="https://kindlegen.s3.amazonaws.com/AmazonKindlePublishingGuidelines.pdf"
      xmlns:idx="https://kindlegen.s3.amazonaws.com/AmazonKindlePublishingGuidelines.pdf">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <style>
      dt {
          font-weight: bold;
      }
      dd {
          margin: 0;
          padding: 0 0 0.5em 0;
          display: block
      }
    </style>
  </head>
  <body>
    <mbp:framset>
     `[PUT THE WORDS HERE]`
      <idx:entry name="default" scriptable="yes" spell="yes">
        <h5>
          <dt>
            <idx:orth>WORD HERE</idx:orth>
          </dt>
        </h5>
        <dd>DEFINITION</dd>
      </idx:entry>
      <hr/>
    </mbp:framset>
  </body>
</html>
```

The `[PUT THE WORDS HERE]` part gets filled in with the markup for all of your words.
The basic structure of a word's entry looks like the following.

```html
<idx:entry name="default" scriptable="yes" spell="yes">
  <h5><dt><idx:orth>Aba</idx:orth></dt></h5>
  <dd>A loose, usually black robe worn by Fremen women and Bene Gesserit sisters.</dd>
</idx:entry>
<hr>
```

Every word has an `<idx:entry>` block followed by a `<hr>`.
These two elements together comprise a single entry.

The `name` attribute on the `<idx:entry>` element defines what lookup index the entry is associated with.
Unless you are building a dictionary with multiple indexes, you can pretty much ignore it.
It should be set to something though and this will need to match up with an entry in the `.opf` file we'll make later.

The `scriptable` attribute makes the entry available from the index and can only have the value `"yes`.
The `spell` can also only be `"yes"` and enables wildcard search and spell correction.

The markup you use inside the `idx:entry` element is mostly up to you.
You do need the `<idx:orth>` tag and its content is the word that is looked up.
The rest of the taags can be whatever you want.
I use `dt` and `dd` because it makes sense to me but I'll admit the `h5` is just there for styling.

You can do some fancier stuff here as well.
This includes specifying if an exact match is required, alternative words that should result in the same lookup, and varying the search word from the displayed word.
Most of that isn't important for many dictionaries so I'm not going to elaborate on how to do that.

### 3. Construct a cover page.

This is just a requirement of a Kindle.
Create a html file called `cover.html` and substitute in the appropiate values.

```html
<html>
  <head>
    <meta content="text/html" http-equiv="content-type">
  </head>
  <body>
    <h1>Dune Dictionary</h1>
    <h3>Created by Jake McCrary</h3>
  </body>
</html>
```

### 4. Create a copyright page

This is also a requirement of the Kindle publishing guide.
There isn't any special markup for doing this.

Just make another html file and fill in some appropiate details.

### 5. Create a usage page

This isn't a requirement but I include another page that explains how to use the dictionary.
Again, this is just an html document with some content in it.

### 5. Make a `.opf` file.

This is one of the poorly documented but extremely important parts of making a Kindle dictionary.

Make a opf file and name it whatever you want; in this example we'll go with `dict.opf`.
This is an xml file that explains how to combine the various files together to actually make a dictionary.

Below is the one I've  used for the Diaspora dictionary.

```xml
<?xml version="1.0"?>
<package version="2.0" xmlns="http://www.idpf.org/2007/opf" unique-identifier="BookId">
  <metadata>
    <dc:title>A dictionary for Diaspora by Greg Egan</dc:title>
    <dc:creator opf:role="aut">Jake McCrary</dc:creator>
    <dc:language>en-us</dc:language>
    <meta name="cover" content="my-cover-image" />
    <x-metadata>
      <DictionaryInLanguage>en-us</DictionaryInLanguage>
      <DictionaryOutLanguage>en-us</DictionaryOutLanguage>
      <DefaultLookupIndex>default</DefaultLookupIndex>
    </x-metadata>
  </metadata>
  <manifest>
    <!-- <item href="cover-image.jpg" id="my-cover-image" media-type="image/jpg" /> -->
    <item id="cover"
          href="cover.html"
          media-type="application/xhtml+xml" />
    <item id="usage"
          href="usage.html"
          media-type="application/xhtml+xml" />
    <item id="copyright"
          href="copyright.html"
          media-type="application/xhtml+xml" />
    <item id="content"
          href="content.html"
          media-type="application/xhtml+xml" />
  </manifest>
  <spine>
    <itemref idref="cover" />
    <itemref idref="usage" />
    <itemref idref="copyright"/>
    <itemref idref="content"/>
  </spine>
  <guide>
    <reference type="index" title="IndexName" href="content.html"/>
  </guide>
</package>
```

An import thing in this is that you have your language markup correct and the `<DefaultLookupIndex>` node needs to contain the same name from the `<idx:entry>` `name` attribute.
The `<DictionaryInLanguage>` and `<DictionaryOutLanguage>` tell the Kindle the valid languages for your dictionary.

The other nodes in the `<metadata>` should be pretty self-evident.

The `<manifest>` gives identifiers for the various files you've made in the previous steps.

You'll notice my `<manifest>` section also has a commented out image.
The official Kindle publishing guidelines require a cover image but in practice, at least for sideloading dictionaries, this isn't required.
If you have one, it will show up and look nice.
If you don't, well, it still looks good enough.

The `<spine>` section references the `<item>`s from the `<manifest>` and specifies the order they appear in your book.

I honestly don't remember why the `<guide>` section is in there or what it is doing in this example.
I'm guessing that is what causes there to be an index with the wordlist in the dictionary but I haven't tried removing it and the documentation doesn't talk about it.

### 6. Combine them together using `kindlegen`.

This is what the current (as of October 2020) Kindle publishing guidelines say to do but Amazon doesn't offer `kindlegen` as a download anymore.
If you really want to use `kindlegen` you still can and I did just do verify I could.
You can still find `kindlegen` throught archived versions of Amazon's download page.

The old OS X version would no longer work on my Apple laptop so I downloaded the Linux version and got it working in a docker container.
I'm not going to go into this though as you can actually use the Kindle Previewer application to make your dictionary.

### (actual) 6. Use the Kindle Previewer application to make the dictionary

1. Download the [Kindle Previewer](https://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000765261) application.
1. Open it up and click `File > Open`.
1. Find your `dict.opf` file and open that.
1. `File > Export` and export it as a `.mobi` file.

The conversion log will complain about a couple things such as missing cover.
As long as these are just `Warnings` it doesn't really matter.

I've found that the previews in this app don't actually match what it looks like on your device so take them with a grain of salt.

### 7. Put it onto your device

Finally, put that dictionary onto your Kindle.
You can do this by either using a USB cable or by emailing it to your Kindle's email address.

Once it is on your Kindle, open it up and double check that the formatting is alright.
Go into the book you've made it for and try looking up a word.
If you didn't get into your dictionary, use the dictionary pop up to change to use yours.
Now when you try to look up a word, your dictionary is searched first.

The great thing is that if a word _isn't_ in your dictionary than the other dictionaries[^1] for that language are searched.
This feature is great as it lets your dictionary be very focused.
Hopefully Amazon does't remove this feature.

[^1]: No idea if it searches all of them in some order but I'm very glad it works this way.


## End

It was interesting creating another dictionary so long after I made my first couple.
The markup has changed over the years but my [Dune dictionary](https://gumroad.com/l/dune-dictionary) still works.
Some of the new features, like the ability to require an exact word match, would have been useful for my [second dictionary](/blog/2013/07/09/releasing-the-functional-javascript-companion/).

The Kindle documetnation is much better but it still isn't great.
It is so much nicer than it was in 2013 though.
It is pretty sad to no longer have `kindlegen` as I don't think you can use the tools Amazon publishes on a Linux machine.

If you're ever in a situation where you think a custom dictionary would be useful, feel free to reach out[^2].

[^2]: I haven't shared my generation code here as I think it is less interesting than talking about what is required to make a dictionary.

Go forth and make dictionaries.

