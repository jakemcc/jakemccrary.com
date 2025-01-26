---
dated-url: true
layout: post
title: Using Emacs to Explore an HTTP API
date: 2014-07-04 13:05
comments: true
published: true
categories:
- Emacs
---

Recently I rediscovered an Emacs package that allows you to interact
with HTTP endpoints from the comfort of an Emacs buffer.
[restclient.el](https://github.com/pashky/restclient.el) provides
`restclient-mode`. This mode allows you to write and execute HTTP
requests in an Emacs buffer. This package can be found in
[MELPA](http://melpa.milkbox.net/#/restclient).

Below is an example buffer that touches the GitHub API.

``` ruby
:github = https://api.github.com

# get users orgs

GET :github/users/jakemcc/orgs

# rendor markdown

POST :github/markdown

{
  "text" : "## Title"
}

# rendor markdown raw

POST :github/markdown/raw
Content-Type: text/plain

Title
-----
```

The example above has a few interesting snippets. `:github` is an
example of a variable. Lines 8-14 show an example of posting json to
an endpoint. You put the data you want to send below the query. The
last POST shows how to set headers for a request.

The location of your cursor decides what query to execute. Comments
start with `#` and break your document into sections. The query in the
same section as your cursor is the one that is executed. If the cursor
is anywhere on lines 3-6 and I hit `C-c C-c` then Emacs queries GitHub
for my organizations. Below is what pops up in a buffer.

``` javascript

[
    {
        "avatar_url": "https:\/\/avatars.githubusercontent.com\/u\/1826953?",
        "public_members_url": "https:\/\/api.github.com\/orgs\/speakerconf\/public_members{\/member}",
        "members_url": "https:\/\/api.github.com\/orgs\/speakerconf\/members{\/member}",
        "events_url": "https:\/\/api.github.com\/orgs\/speakerconf\/events",
        "repos_url": "https:\/\/api.github.com\/orgs\/speakerconf\/repos",
        "url": "https:\/\/api.github.com\/orgs\/speakerconf",
        "id": 1826953,
        "login": "speakerconf"
    },
    {
        "avatar_url": "https:\/\/avatars.githubusercontent.com\/u\/4711436?",
        "public_members_url": "https:\/\/api.github.com\/orgs\/outpace\/public_members{\/member}",
        "members_url": "https:\/\/api.github.com\/orgs\/outpace\/members{\/member}",
        "events_url": "https:\/\/api.github.com\/orgs\/outpace\/events",
        "repos_url": "https:\/\/api.github.com\/orgs\/outpace\/repos",
        "url": "https:\/\/api.github.com\/orgs\/outpace",
        "id": 4711436,
        "login": "outpace"
    }
]
// HTTP/1.1 200 OK
// Server: GitHub.com
// Date: Fri, 04 Jul 2014 17:34:26 GMT
// Content-Type: application/json; charset=utf-8
// other headers removed for space consideration on blog

```


`C-c C-c` triggers `restclient-http-send-current` which runs a query
and pretty prints the result. I could have used `C-c C-r` to trigger
`restclient-http-send-current-raw` which executes a query and shows
the raw result.

It isn't a perfect mode. One issue I've come across is that queries
targeting `localhost` fail. The solution is to query `127.0.0.1`.

`restclient-mode` makes Emacs a useful tool for exploring and
testing HTTP APIs. Since it operates on a simple text
format it allows you to easily share executable documentation with
others. I highly recommend **restclient.el**.