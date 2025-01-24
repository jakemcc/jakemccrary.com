---
dated-url: true
layout: post
title: Speeding up this site by optionally loading Disqus comments
date: 2017-06-30 19:37 -0500
comments: true
published: true
description: Optionally loading Disqus comments drastically reduced how much data
  readers of this site download.
keywords: disqus, speed, load times
categories:
- blog
---

Earlier this month I took another look at what was required for reading an article on this site. What [else](/blog/2016/04/30/speeding-up-my-blog/) could I do to make this site load faster?

To do this, I loaded up [WebPageTest](http://webpagetest.org/) and pointed it towards one of my [posts](/blog/2017/05/30/adding-a-json-feed-to-octopress-slash-jekyll/). To my shock, it took 113 requests for a total of 721 KB to load a single post. This took WebPageTest 6.491 seconds. The document complete event triggered after 15 requests (103 KB, 1.6 seconds).

113 requests to load a static article was ridiculous. Most of those requests happened as a result of loading the Disqus javascript. I find comments valuable and want to continue including them on my site. Because of this, I couldn't remove Disqus. Instead, I made loading Disqus optional.

After making the required changes, it only takes 11 requests for 61 KB of data to fully load the test post. The document complete event only required 8 requests for 51 KB of data. Optionally loading the Disqus javascript resulted in a massive reduction of data transferred.

How did I do it? The template that generates my articles now only inserts the Disqus javascript when a reader clicks a button. My [final template](https://github.com/jakemcc/jakemccrary.com/blob/74f4232ce7263ba3de48497d0c0d10a8fa1a73f9/source/_includes/disqus.html) is at the bottom of this post.

The template adds an `insertDisqus` function that inserts a `<script>` element when a reader clicks a button. This element contains the original JavaScript that loads Disqus. When the `<script>` element is inserted into the page, the Disqus javascript is loaded and the comments appear.

My exact template might not work for you, but I'd encourage you to think about optionally loading Disqus and other non-required JavaScript. Your readers will thank you.

{% raw %}
```html
{% if site.disqus_short_name and page.comments == true %}
  <noscript>Please enable JavaScript to view the <a href="http://disqus.com/?ref_noscript">comments powered by Disqus.</a></noscript>
  <div id="disqus_target">
    <script>
     var insertDisqus = function() {
       var elem = document.createElement('script');
       elem.innerHTML =  "var disqus_shortname = '{{ site.disqus_short_name }}'; var disqus_identifier = '{{ site.url }}{{ page.url }}'; var disqus_url = '{{ site.url }}{{ page.url }}'; (function () {var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true; dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js'; (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);}());"
       var target = document.getElementById('disqus_target');
       target.parentNode.replaceChild(elem, target);
     }
    </script>
    <button class="comment-button" onclick="insertDisqus()"><span>ENABLE COMMENTS AND RECOMMENDED ARTICLES</span></button>
  </div>
{% endif %}
```
{% endraw %}