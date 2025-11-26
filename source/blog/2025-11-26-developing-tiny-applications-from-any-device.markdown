---
layout: post
title: Developing tiny applications from any device
date: 2025-11-26 09:56 -0600
comments: true
published: false
description: TODO
categories:
- ai
- programming
- inspiration
---

I'm a big fan of making [small (sometimes silly) programs](/blog/2020/10/03/go-create-silly-small-programs/).
You can make your life a bit better by recognizing when some specific piece of software is missing and, if scoped small enough, you can cause that software to exist without too much time investment.

When you develop the practice of recognizing when a bit of software would be helpful, you see opportunities all the time.
You don't control when you get inspiration for these programs.
So you come up with strategies for handling these bursts of inspiration.

One strategy: Write yourself a note (paper, email, favorite note taking or TODO app) and maybe get around to it later.
(You occasionally manage to get around to it later.)
Another valid strategy: Think about the inspiration and then let go of it under the idea that it will bubble back up when appropriate.

My new strategy:
1. Inspiration strikes!
2. I pull out my phone and open my web browser to OpenAI's Codex web app.
3. I translate my inspiration to a task and type (or voice-to-text) it into Codex
4. I submit the task to Codex, go about my day, and check on it later.
5. Later: read the diff, click a button to open a PR, merge the PR, and let GitHub Actions deploy the changes to GitHub Pages.

AI tools lower the bar to creating those small, specific programs and lowers the bar to iterating on those small programs.

Since early summer 2025, I've been able to develop and iterate on a handful of single-page web applications this way.
As models improve, the bar is getting even lower.

Here is my setup:

- I have a single repo named experiments[^1] on GitHub.
- This repo has a subdirectory per application.
- The applications are in a variety of web languages (html, css, typescript, javascript, clojurescript).
- OpenAI Codex is linked with this experiments repo.

With this setup, I'm able to follow the above strategy with very minimal friction.
If I have an idea for a new little application, I open Codex and provide a description of what I want and what it should be called and it usually manages to start work on it.
When I have an idea for tweaking an application, I open Codex, tell it what subdirectory the app is in and what tweak I want made.

When Codex is done, I do a quick scan through the diff, click the buttons to open a PR, merge it, wait for the deploy, and then check on the deployed artifacts.

All of this can be done from a cellphone.

[^1]: I don't know if this limitation still exists but when I was initially setting this up my experiments repo had zero commits. This caused problems in Codex that were fixed by adding a single commit.
