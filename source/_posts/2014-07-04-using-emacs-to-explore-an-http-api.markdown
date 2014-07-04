---
layout: post
title: "Using Emacs to Explore an HTTP API"
date: 2014-07-04 11:13
comments: true
published: false
categories:
- Emacs
---

Recently I rediscovered an Emacs package that lets you interact with
HTTP endpoints from the comfort of an Emacs buffer.
[restclient.el](https://github.com/pashky/restclient.el) provides
`restclient-mode`. This mode allows you to write and execute HTTP
requests in an Emacs buffer. This package is on
[MELPA](http://melpa.milkbox.net/#/restclient).

Below is an example buffer that touches the GitHub API.

``` ruby :github = https://api.github.com

# get users orgs

GET :github/users/jakemcc/orgs

# rendor markdown

POST :github/markdown { "text" : "# Title" }

# rendor markdown raw

POST :github/markdown/raw Content-Type: text/plain

Title ------ ```

The example above has a few interesting snippets in it. The `:github`
above is an example of a variable. Lines 8-13 show an example of
posting json to an endpoint. You put whatever data you want to post
below the query. The POST on line 16 shows how you can set headers for
a request.

The location of your cursor is what decides what query is run.
Comments (lines starting with `#`) break your document into sections. The
query in the same section as your cursor is the one that is run. If I
place my cursor anywhere in lines 3-6 and hit `C-c C-c` the below
buffer pops up showing I'm in the
[speakerconf](http://speakerconf.com/) and
[Outpace](http://outpace.com/) organizations.

``` javascript [

    { "avatar_url": "https:\/\/avatars.githubusercontent.com\/u\/1826953?", "public_members_url": "https:\/\/api.github.com\/orgs\/speakerconf\/public_members{\/member}", "members_url": "https:\/\/api.github.com\/orgs\/speakerconf\/members{\/member}", "events_url": "https:\/\/api.github.com\/orgs\/speakerconf\/events", "repos_url": "https:\/\/api.github.com\/orgs\/speakerconf\/repos", "url": "https:\/\/api.github.com\/orgs\/speakerconf", "id": 1826953, "login": "speakerconf" }, { "avatar_url": "https:\/\/avatars.githubusercontent.com\/u\/4711436?", "public_members_url": "https:\/\/api.github.com\/orgs\/outpace\/public_members{\/member}", "members_url": "https:\/\/api.github.com\/orgs\/outpace\/members{\/member}", "events_url": "https:\/\/api.github.com\/orgs\/outpace\/events", "repos_url": "https:\/\/api.github.com\/orgs\/outpace\/repos", "url": "https:\/\/api.github.com\/orgs\/outpace", "id": 4711436, "login": "outpace" } ]
        // HTTP/1.1 200 OK // Server: GitHub.com // Date: Fri, 04 Jul
        2014 16:41:28 GMT // Content-Type: application/json;
        charset=utf-8 // more headers have been removed for space
        considerations ```

`C-c C-c` runs the query and pretty prints the result. I could
have used `C-c C-r` to run the query and show the raw result.

It isn't a perfect mode. One issue I've come across is that queries
targeting `localhost` fail. The solution is to query `127.0.0.1`.

**restclient-mode** makes Emacs a useful tool for exploring and
testing HTTP APIs. Since it operates on a simple text
format it allows you to easily share executable documentation with
others. If you use Emacs I highly recommend **restclient.el**.
