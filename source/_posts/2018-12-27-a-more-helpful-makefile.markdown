---
layout: post
title: "A more helpful makefile"
date: 2018-12-27 20:48:59 -0600
comments: true
published: false
description: PUT SUMMARY HERE 
keywords: 'makefile'
categories: 
- make
- command-line
---

In an [older article](/blog/2016/11/30/unify-your-project-interfaces/) of mine I extolled the virtues of having unified interfaces for interacting with your projects. I recently started working at Coinbase and the group I'm working with is mostly using makefiles as that common interface. We still have some more work to go for actually unifying all the various projects but I've picked up one tip that makes switching between projects easier.

That tip is to have the default target of your makefile be one that prints out a helpful message. This looks like the following.

```
.PHONEY: help
help:
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
	 sort | \
	 awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
```

There is a lot going on there but it basically looks through your makefile targets and finds the ones that have a comment starting with `##`. Those targets are printed to the console along with the comment.

As an example, the makefile for my website looks similar to the below file.

```
.PHONEY: help
help:
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
	 sort | \
	 awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONEY: watch
watch: ## Watch for changes and serve preview of site with drafts
	bundle exec rake clean
	bundle exec rake preview

.PHONEY: develop
develop: ## Serve a preview of the site without drafts and refresh changes
	bundle exec rake clean
	bundle exec rake develop

.PHONEY: new_adventure
new_adventure: ## Start a new adventure post
	bundle exec rake new_adventure

.PHONEY: new_post
new_post: ## Start a new post
	bundle exec rake new_post 

.PHONEY: deploy
deploy: ## deploy
	./deploy.sh
```

When this file, when I run `make` in this websites source, I get the following output.

```
0 [last: 0s] 21:11:50 ~/src/jakemcc/blog (master)
$ make
deploy                         deploy
develop                        Serve a preview of the site without drafts and refresh changes
new_adventure                  Start a new adventure post
new_post                       Start a new post
watch                          Watch for changes and serve preview of site with drafts
```

This is super useful when you're starting doing work in a new project. With this feature you can get a quick list of useful targets and a description. It allows you to quickly see what can be done in a project.
