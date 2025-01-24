---
dated-url: true
layout: post
title: "Identifying similar text documents"
date: 2019-05-30 13:40:35 -0500
comments: true
published: false
description: PUT SUMMARY HERE 
keywords: 'csv, keywords, here'
categories: 
---

I've infrequently wondered how repetative I am in the various articles I write on my website.

Luckily, this curiousity lines up with one of the problems we solved at Lumanu. At the time, Lumanu was indexing online articles we crawled and helping people do better content marketing. Part of that was allowing people to search and see articles that were relevant to that search. It was important to us to not show a bunch of very similar articles to any given search.

We did this at Lumanu by 






Years ago, I was heading up the technology team at a small content marketing startup. In one version of our product, we crawled the web for articles and used that data to connect clients to authors. It also functioned as a search engine over the content we crawled.

One way we made our search better was to identify similar articles and only display one article instead of all the very similar results. This was a much nicer user experience.

We did this by using [locality-sensitive hashing](https://en.wikipedia.org/wiki/Locality-sensitive_hashing). Locality-sensitive hashing is a category of hashing where collisions are maximized. Similar items will hash to similar values.

More specifically we used the [simhash](https://en.wikipedia.org/wiki/SimHash) technique for finding near duplicate articles. 

# What is cosine similarity?
# What is simhash?
# Applying it to articles on this website
# End