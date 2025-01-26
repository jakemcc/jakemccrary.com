---
dated-url: true
layout: post
title: Adding a JSON Feed to an Octopress/Jekyll generated site
date: 2017-05-30 22:31 -0500
comments: true
published: true
description: Quick guide to adding a JSON Feed to your Octopress/Jekyll generated
  site
keywords: json feed, json, jekyll, octopress, liquid
categories:
- blog
---

I went to a coffee shop this last weekend with the intention of writing up a [quick article](/blog/2017/05/29/using-comm-to-verify-matching-content/) on `comm`. I sat down, sipping my coffee, and wasn’t motivated. I didn’t feel like knocking out a short post, and I didn’t feel like editing a draft I’ve been sitting on for a while. I wanted to do some work though, so I decided to add a [JSON Feed](https://jsonfeed.org/) to this site.

JSON Feed is an alternative to [Atom](https://tools.ietf.org/html/rfc4287) and [RSS](http://cyber.harvard.edu/rss/rss.html) that uses JSON instead of XML. I figured I could add support for it in less than the time it would take to enjoy my coffee and maybe some readers would find it useful. I’d be shocked if anyone actually finds this useful, but it was a fun little exercise anyway.

An old version of Octopress (2.something), which uses an old version of Jekyll (2.5.3), generates this site. Despite this, I don’t think the template would need to change much if I moved to a new version. The template below is saved as [source/feed.json](https://github.com/jakemcc/jakemccrary.com/blob/00d4b0416ab2591be5702286b735091a3d2e2105/source/feed.json) in my git repository.

``` javascript
---
layout: null
---
{
  "version": "https://jsonfeed.org/version/1",
  "title": {{ site.title | jsonify }},
  "home_page_url": "{{ site.url }}",
  "feed_url": "{{site.url}}/feed.json",
  "favicon": "{{ site.url }}/favicon.png",
  "author" : {
      "url" : "https://twitter.com/jakemcc",
      "name" : "{{ site.author | strip_html }}"
  },
  "user_comment": "This feed allows you to read the posts from this site in any feed reader that supports the JSON Feed format. To add this feed to your reader, copy the following URL - {{ site.url }}/feed.json - and add it your reader.",
  "items": [{% for post in site.posts limit: 20 %}
    {
      "id": "{{ site.url }}{{ post.id }}",
      "url": "{{ site.url }}{{ post.url }}",
      "date_published": "{{ post.date | date_to_xmlschema }}",
      "title": {% if site.titlecase %}{{ post.title | titlecase | jsonify }}{% else %}{{ post.title | jsonify }}{% endif %},
      {% if post.description %}"summary": {{ post.description | jsonify }},{% endif %}
      "content_html": {{ post.content | expand_urls: site.url | jsonify }},
      "author" : {
        "name" : "{{ site.author | strip_html }}"
      }
    }{% if forloop.last == false %},{% endif %}
    {% endfor %}
  ]
}
```

I approached this problem by reading the [JSON Feed Version 1 spec](https://jsonfeed.org/version/1) and cribbing values from the template for my Atom feed. The trickiest part was filling in the `"content_html"` value. It took me a while to find figure out that `jsonify` needed to be at the end of `{{ post.content | expand_urls: site.url | jsonify }}`. That translates the post's HTML content into its JSON representation. You’ll notice that any template expression with `jsonify` at the end also isn’t wrapped in quotes. This is because `jsonify` is doing that for me.

The `{% if forloop.last == false %},{% endif %}` is also important. Without this, the generated JSON has an extra `,` after the final element in items. This isn’t valid JSON.

I caught that by using the command line tool [json](http://trentm.com/json/). If you ever edit JSON by hand or generate it from a template then you should add this tool to your toolbox. It will prevent you from creating invalid JSON.

How did I use it? I’d make a change in the `feed.json` template and generate an output file. Then I’d `cat` that file to `json --validate`. When there was an error, I’d see a message like below.


``` console
0 [last: 5s] 12:43:47 ~/src/jakemcc/blog (master *)
$ cat public/feed.json | json --validate
json: error: input is not JSON: Expected ',' instead of '{' at line 25, column 5:
            {
        ....^
1 [last: 0s] 12:43:49 ~/src/jakemcc/blog (master *)
$
```

And there would be zero output on success.

``` console
0 [last: 5s] 12:45:25 ~/src/jakemcc/blog (master)
$ cat public/feed.json | json --validate
0 [last: 0s] 12:45:30 ~/src/jakemcc/blog (master)
$
```

It was pretty straightforward to add a JSON Feed. Was it a good use of my time? `¯\_(ツ)_/¯`. In the process of adding the feed I learned more about Liquid templating and figured out how to embed liquid tags into a blog post. Even adding redundant features can be a useful exercise.
