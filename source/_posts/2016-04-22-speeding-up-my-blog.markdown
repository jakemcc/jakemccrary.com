---
layout: post
title: "Speeding up my blog"
date: 2016-04-22 19:34:05 -0500
comments: true
published: false
description: In an effort to improve my readers' experience I made my Octopress blog load significantly faster.
keywords: 'octopress, blog, writing, javascript'
categories: 
- blog
---

I was recently reading
[Jeff Ramnani's](https://jefframnani.com/about/) _about_ page and I
was somewhat inspired by it. It loads quickly and links to
[Designing blogs for readers](http://mattgemmell.com/designing-blogs-for-readers/),
an interesting essay by Matt Gemmmell. Reading that essay inspired me
to think about my own site and what experience I want to deliver to
readers.

I can't imagine what experience every reader wants but I know what I
want to experience when I read an article online. Good content is most
important. Beyond that I prefer when a page loads fast and when the
visual design doesn't get in the way. I think a great example of these
two requirements is [zen habits](http://zenhabits.net/falling/) (along
with Jeff's blog above and Matt Gemmell's).

My own site sort of achieves those goals. I like to think I'm writing
well-written content that helps others. I know it has helped me. With
regards to clean design I think there is room for improvement. I don't
think my site's design is actively distracting from the content
though, so I've decided to focus on improving the page load time
first.

As with any optimization problem it is important figure what you're
going to measure, how you're going to measure it and your starting
point. I decided to focus on my page load time, as measured by
[Web Page Test](http://www.webpagetest.org). I used Google's
[PageSpeed Insights](https://developers.google.com/speed/pagespeed/insights/)
to score and provide helpful tips for improving page
speed. Unfortunately I didn't capture my starting point with PageSpeed
Insights but I think I was scoring around a 66/100 for mobile and
79/100 for desktop.

![Starting point from Web Page Test](/images/before-optimizations.png)

As measured by Web Page Test, the first load of my main page took five
seconds and it wasn't fully loaded for another second. This is
ridiculous. My page is almost entirely static content and most of my
assets are served from CloudFlare.

Next I looked at what was actually being loaded. Google's PageSpeed
Insights identified that I had three render-blocking `script`
tags. The offending scripts were Modernizr, jQuery, and
octopress.js. PageSpeed Insights
[recommends](https://developers.google.com/speed/docs/insights/BlockingJS#overview)
inlining JavaScript required to render the page or make loading
asynchronous. I decided to go a step further and remove the need for
the JavaScript.

### Removing octopress.js

It turns out `octopress.js` was the reason Modernizr and jQuery were
required. Most of what
[octopress.js](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js)
did were things that I don't need; some sort of
[flash video fallback](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L111),
adding line numbers to
[GitHub Gists](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L112),
[rendering](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L99)
delicious links, and toggling the
[sidebar visibility](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L114). I
was able to delete all that code.

Next up was the
[mobile navigation](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L1-L13)
`octopress.js` provided. This feature enabled navigation through a
`<select>` element when the reader's view port was tiny. Restyling my
navigation bar to fit better on small screens allowed me to remove
this feature. `ocotpress.js` also did some
[feature detection](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L37)
for Modernizr. I stopped using image masks and was able to remove that
code as well. 

The remaining code in `octopress.js` was a workaround for an
[iOS scaling bug](https://github.com/jakemcc/jakemccrary.com/blob/c27a131aef437181dcab9552c3241f8adafb3884/source/javascripts/octopress.js#L121-L136). This
JavaScript was inlined into my html. At this point `octopress.js` was
empty and with it empty the requirements for jQuery and Modernizer
disappeared. This let me remove three render-blocking `script` tags.

### Remaining JavaScript

At this point the remaining JavaScript used for my blog was enabling
comments with Disqus and showing recent tweets in my sidebar. I still
enjoy having comments on my blog so I'm keeping Disqus around. I doubt
that readers care what my most recent tweets are so I removed
Twitter's JavaScript.

### Nearly no JavaScript, now what?

At this point Google's PageSpeed Insight was suggesting that I up my
cache times, inline my css, and move my web fonts lower on my
page. Bumping up my cache times was trivial; I simply tweaked a
CloudFlare setting. 

I opted to not inline my css. This would require me to modify my
site's generation and I just didn't feel like diving down that rabbit
hole. I also didn't move the web fonts lower on the page. I find font
flashing jarring and will be attempting to get rid of them entirely
sometime in the future.

### Results

I used Web Page Test to measure again and now the page load time is
down to 2.5 seconds. Page load times are cut in half from the starting
point. My PageSpeed Insights scores are also higher; up to 79/100 for mobile
and 92/100 for desktop.

![Web Page Test after optimization](/images/after-optimizations.png)

Honestly, that number still seems high[^1] to me and I'm sure I could
get it lower. But for now it is good enough[^2]. As a result of this
exploration I've learned more about my blogging setup and managed to
speed up my page load. Now it is time to go back to focusing
researching for future posts (and at some point restyling).

[^1]: I'm asking Web Page Test to load my page using IE10. I get much faster load times using Chrome or Firefox locally which is what most of my readers use. This is good enough for now.

[^2]: I mean, the starting point was probably good enough but if I admitted that then I wouldn't have had the excuse to dig into my site's load time.
