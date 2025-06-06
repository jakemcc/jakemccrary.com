---
dated-url: true
layout: post
title: Unify your project interfaces
date: 2016-11-30 17:51 -0600
comments: true
published: true
description: Make your life easier and unify the command line interface to working
  on your projects.
keywords: programming, makefile
categories:
- command-line
- make
---

Jeff Ramnani wrote an [article](https://jefframnani.com/project-build-protocol/) about unifying your command line interactions across programming projects.
I recommend that you read it.
The basic gist is that we often find ourselves working on multiple projects at a time.
Frequently these projects are in different languages and use different build tools.
Remembering the necessary incantations to interact with the various projects is difficult and we're lazy.
We can do better by standardizing an interface to our projects.

This interface can take many forms.
One option is to have a `bin` or `scripts` directory in each project and then consistently name the scripts you put in there (examples: `run`, `test`, and `build`).
Another option is to use Makefiles with consistently named targets.
Either way, your projects now have a standard way of interacting with them.
This frees you from having to remember all the various commands and makes onboarding new developers easier.

I've been using a similar approach to Jeff Ramnani for years and highly recommend it.
I'm a fan of the Makefile approach but either approach works.
The unified targets I use across projects are the following:

- `up` - Brings the system up
- `status` - Is the system up and running?
- `logs` - Show me the logs
- `local-db` - Connect to my local database
- `build` - Build the project
- `test` - Run the tests

If you haven't created a common interface for your projects I recommend that you do it.
It definitely makes moving between projects easier.