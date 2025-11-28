---
layout: post
title: Developing tiny applications from any device
date: 2025-11-26 09:56 -0600
comments: true
published: false
description: How I create tiny web apps from anywhere using my phone.
categories:
- ai
- programming
- inspiration
---

I'm a big fan of making [small (sometimes silly) programs](/blog/2020/10/03/go-create-silly-small-programs/).
As a software developer, you have a superpower: you can identify problems in your life and fix them by creating some specific software that solves for exactly what you need.
When scoped small enough, these tiny programs take minimal time investment to create.

When you develop the practice of recognizing when a bit of software would be helpful, you see opportunities all the time.
But you don't control when you get inspiration for these programs.
So you come up with strategies for handling these bursts of inspiration.

One strategy: Write yourself a note (paper, email to yourself, some app on your phone) and maybe get around to it later.
(You occasionally manage to get around to it later.)
Another strategy: Think about the inspiration and trick yourself into thinking you'll remember it later when you're at a computer.
You justify this by claiming if you forget it, it must not have been important.

My new strategy:
1. Inspiration strikes!
2. I pull out my phone and open my web browser to [OpenAI's Codex web app](https://openai.com/codex/).
3. I translate my inspiration into a task and type (or voice-to-text) it into Codex.
4. I submit the task to Codex, go about my day, and check on it later.
5. Later: read the diff, click the Codex button to open a PR, merge the PR through GitHub's mobile interface, and let GitHub Actions deploy the changes to GitHub Pages.

I started using this technique in early summer 2025.
Since then, I've been able to develop and iterate on a handful of single-page web applications this way.
As models improve, it is getting even easier to knock them out.

Here is my setup:

- I have a single repo named [experiments](https://github.com/jakemcc/experiments)[^1] on GitHub.
- This repo has a subdirectory per application.
- The applications are in a variety of web languages (HTML, CSS, TypeScript, JavaScript, ClojureScript).
- OpenAI Codex is linked with this experiments repo.

With this setup, I'm able to follow the above strategy with very minimal friction.
If I have an idea for a new little application, I open Codex and provide a description of what I want and what it should be called, and it usually manages to start work on it.
When I have an idea for tweaking an application, I open Codex, tell it what subdirectory the app is in and what tweak I want made.
All of this can be done from a cellphone.

When Codex is done, I do a quick scan through the diff, click the buttons to open a PR, merge it, wait for the deploy, and then check on the deployed artifacts.
The apps end up published at [jake.in/experiments](https://jake.in/experiments).

It isn't all smooth; sometimes a problem is introduced.
Depending on the problem, I'll either revert the code and try again or give Codex more instructions and try to have it fix it.
If really needed, I'll fire up my laptop and fix it myself or iterate with AI on fixing the problem there.

The bar has been seriously lowered for creating specific software.
Go do it.
It is fun, but in a different way than traditional programming.

[^1]: I don't know if this limitation still exists, but when I was initially setting this up my experiments repo had zero commits. This caused problems in Codex that were fixed by adding a single commit.
