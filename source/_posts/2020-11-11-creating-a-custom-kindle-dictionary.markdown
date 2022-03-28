---
layout: post
title: Creating a custom Kindle dictionary
date: 2020-11-11 19:23 -0600
comments: true
published: true
description: Here are the steps to make a custom Kindle dictionary.
keywords: kindle, dictionary
categories: 
- kindle
- dictionary
---

Back in April 2013, I created and published a [custom Kindle dictionary](https://gumroad.com/l/dune-dictionary) for the book [Dune](http://www.amazon.com/gp/product/B00B7NPRY8/ref=as_li_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B00B7NPRY8&linkCode=as2&tag=jakemccrary08-20&linkId=LC2NFEXWA7JXW57B).
As far as I can tell, [my Dune dictionary](https://gumroad.com/l/dune-dictionary) was the very first custom Kindle dictionary for a fiction book.

I created it because I was reading Dune for the first time and there were many unfamiliar words.
These words could not be looked up by my Kindle because they were not found in any of on-device dictionaries.
These words were in Dune's glossary but flipping back-and-forth to that on a Kindle was a huge pain.

I initially worked around this by printing a word list from Wikipedia and carrying it with me.
This was better but it was still annoying.

I was so annoyed that I took a break from reading to figure out how to create a custom Kindle dictionary.
At the time, there wasn't a ton of great information online about how to do this.

Eventually, I found Amazon's [Kindle Publishing Guidelines](https://s3.amazonaws.com/kindlegen/AmazonKindlePublishingGuidelines.pdf) and, referencing it, managed to figure out something that worked.
The link in the previous sentence is to the **current** documentation which is much nicer than the [mid-2013 documentation](https://web.archive.org/web/20130408183149/http://s3.amazonaws.com/kindlegen/AmazonKindlePublishingGuidelines.pdf).
The earlier documentation left me with questions and required quite a bit of experimentation.

Using the mid-2013 documentation, I developed some Clojure code to generate my [dictionary](https://gumroad.com/l/dune-dictionary).
Doing this in 2013 was annoying.
The documentation was not good.

I recently read [Greg Egan's Diaspora](https://www.gregegan.net/DIASPORA/DIASPORA.html) and found myself wishing I had a custom dictionary.
I took a break from reading and packaged up Diaspora's glossary into a dictionary.
I could have stuck with my 2013 generator but I decided to update it and write this article about creating a Kindle dictionary in late 2020.

The new documentation is a bit better but it still isn't great.
Here is what you need to do.

## Making a dictionary

Below are the steps to building a dictionary.

1. Construct your list of words and definitions.
1. Convert the list into the format specified by Amazon.
1. Create a cover page.
1. Create a copyright page.
1. Create a usage page (definitely optional).
1. Make an `.opf` file.
1. Combine the files together.
1. Put it onto your device.

### 1. Construct your list of words and definitions

There really are no set instructions for this.
Source your words and definitions and store them in some format that you'll be able to manipulate in a programming language.

I've sourced words a few different ways.
I've taken them straight from a book's glossary, a Wikipedia entry, and extracted them from a programming book's [source code](/blog/2013/07/09/releasing-the-functional-javascript-companion/).

### 2. Convert the list into the format specified by Amazon

Below is the basic scaffolding of the html file Amazon requires along with some inline styles that I think look decent on devices.
This has some extra stuff in it and also doesn't contain everything Amazon specifies.
But it works.

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
      h5 {
          font-size: 1em;
          margin: 0;
      }
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
    <mbp:frameset>
      [PUT THE WORDS HERE]
    </mbp:frameset>
  </body>
</html>
```

The `[PUT THE WORDS HERE]` part gets filled in with the markup for all of your words.
The basic structure for an entry looks like the following.

```html
<idx:entry name="default" scriptable="yes" spell="yes">
  <h5><dt><idx:orth>WORD HERE</idx:orth></dt></h5>
  <dd>DEFINITION</dd>
</idx:entry>
<hr/>
```

Every word has an `<idx:entry>` block followed by a `<hr>`.
These two elements together comprise a single entry.

The `name` attribute on the `<idx:entry>` element sets the lookup index associated with the entry.
Unless you are building a dictionary with multiple indexes, you can pretty much ignore it.
Whatever value is provided needs to match the value found in the `.opf` file we'll make later.

The `scriptable` attribute makes the entry available from the index and can only have the value `"yes"`.
The `spell` can also only be `"yes"` and enables wildcard search and spell correction.

The markup you use inside the `idx:entry` element is mostly up to you.
The only markup you need is the `<idx:orth>` node.
Its content is the word being looked up.
The rest of the markup can be whatever you want.

I wrap the term in a `dt` and the definition in `dd` because it just feels like the right thing to do and provides tags to put some CSS styles on.
I wrap the `dt` element in an `h5` because I couldn't figure out what CSS styles would actually work on my Kindle voyage to put the term on its own line.

It isn't that I don't know what the styles should be but my Kindle did not respect them.
Figuring out stuff like this is part of the experimentation required to produce a dictionary that you're happy with.

There is additional supported markup that provides more functionality.
This includes providing alternative words that all resolve to the same entry, specifying if an exact match is required, and varying the search word from the displayed word.
Most dictionaries don't need these features so I'm not going to elaborate on them.

### 3. Construct a cover page.

This is just a requirement of a Kindle.
Create a html file called `cover.html` and substitute in the appropriate values.

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

Amazon wants you to provide an image as well but you don't actually have to do this.
You probably need to do this if you actually publish the dictionary through Amazon[^3].

[^3]: This is actually a challenge to do due to restrictions on what Amazon allows published.

### 4. Create a copyright page

This is also a requirement of the Kindle publishing guide.
There isn't any special markup for doing this.

Just make another html file and fill in some appropriate details.

### 5. Create a usage page

This isn't a requirement but I include another page that explains how to use the dictionary.
Again, this is just a html document with some content in it.

### 6. Make an `.opf` file.

This is one of the poorly documented but extremely important parts of making a Kindle dictionary.
This is a XML file that ties together all the previous files into an actual dictionary.

Make an opf file and name it whatever you want; in this example we'll go with `dict.opf`.

Below is the one I've used for the Diaspora dictionary.
If you've created an image for a cover then lines 7 and 15 are the important and line 15 should be uncommented.

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

An import element in this file is the `<DefaultLookupIndex>` element.
The `<DefaultLookupIndex>` content needs to contain the same value from the `name` attribute on your `<idx:entry>` elements.
The `<DictionaryInLanguage>` and `<DictionaryOutLanguage>` tell the Kindle the valid languages for your dictionary.

The other elements in the `<metadata>` should be pretty self-explanatory.

The `<manifest>` gives identifiers for the various files you've made in the previous steps

The commented out `<img>` shows how you'd add the cover image if you opt to have one.
For sideloading dictionaries onto Kindles, it is not required.

The `<spine>` section references the `<item>`s from the `<manifest>` and specifies the order they appear in your book.

I honestly don't remember why the `<guide>` section is in there or what it is doing in this example.
I'm guessing that is what causes there to be an index with the word list in the dictionary but I haven't tried removing it and the documentation doesn't talk about it.
I only have it there since I had it in earlier dictionaries I made.

### 7. Combine the files together

The publishing guidelines (as of October 2020) tell you to combine the previously created files together using the command line tool `kindlegen`.
The problem with those instructions is that Amazon doesn't offer `kindlegen` as a download anymore.
If you want to use it, you can still find it through the Internet Archive.

Instead of following the publishing guidelines, we'll use Kindle Previewer to finish making the dictionary.
It is pretty straight forward.

1. Download the [Kindle Previewer](https://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000765261) application.
1. Open it up and click `File > Open`.
1. Find your `dict.opf` file and open that.
1. `File > Export` and export it as a `.mobi` file.

The conversion log will complain about a couple things such as missing cover.
As long as these are just `Warnings` it doesn't matter.

I've found the preview in this app doesn't match what it looks like on your device so take it with a grain of salt.

### 7. Put it onto your device

Finally, put the dictionary onto your Kindle.
You can do this by either using a USB cable or by emailing it to your Kindle's email address.

Once it is on your Kindle, open it up and double check that the formatting is correct.
Next, open the book you've made it for and try looking up a word.
If the lookup fails or uses another dictionary, click the dictionary name in the pop-up to change your default dictionary to yours.
Now when you try to look up a word, your dictionary is searched first.

The great thing is that if a word _isn't_ in your dictionary then the Kindle searches the other dictionaries[^2].
This feature is great as it lets your dictionary be very focused.
Hopefully Amazon doesn't remove this feature.

[^2]: No idea if it searches all of them in some order but I'm very glad it works this way.


## End

It was interesting creating another dictionary so long after I made my first couple.
Some of the new features, like the ability to require an exact word match, would have been useful for my [second dictionary](/blog/2013/07/09/releasing-the-functional-javascript-companion/).
The actual markup recommendations have changed over the years but luckily my [Dune dictionary](https://gumroad.com/l/dune-dictionary) still works.
I'm not constantly checking that it works, so if Amazon had changed something and it broke, I probably wouldn't notice until someone reported it.

The Kindle documentation is much better now compared to 2013 but it still isn't great.

It is also a bummer that `kindlegen` is gone.
It was nice to be able to convert the input files from the command line.
I also think this means you can no longer make a dictionary from a Linux machine as I don't remember seeing Kindle Previewer support.

If you're ever in a situation where you think a custom dictionary would be useful, feel free to reach out.

Go forth and make dictionaries.
