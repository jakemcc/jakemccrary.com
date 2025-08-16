---
layout: post
title: "Human-in-the-loop AI and 'A Deepness in the Sky'"
date: 2025-08-14 10:00 -0500
comments: true
published: false
description: Riffing on Vernor Vinge’s “ziphead support” and how today’s AI boom echoes the book’s human-in-the-loop automation—minus the dystopia.
keywords: ai, llms, human-in-the-loop, Vernor Vinge, A Deepness in the Sky
categories:
- ai
- chatgpt
- books
---

I was recently reading Vernor Vinge's [A Deepness in the Sky](https://en.wikipedia.org/wiki/A_Deepness_in_the_Sky) and a paragraph in it triggered thoughts about our the AI tooling that more and more folks are finding useful.

**Warning: I'm not going to avoid spoiling parts of that book in this article.**

In _A Deepness in the Sky_, one of the groups of humans, the Emergents, has figured out how to take advantage of a "mindrot" virus that was a plague on their homeland.
The mindrot allows the Emergents to force an obsession in the infected.
This practically turns the infected person, colloquially called a ziphead, into a specialized appliance focused on their obsession and little else.

One of the Emergents talks about what the zipheads enable in the following paragraph:

> They left the group room and started back down the central tower. “See, Pham, you—all you Qeng Ho—grew up wearing blinders. You just know certain things are impossible. I see the clichés in your literature: ‘Garbage input means garbage output’; ‘The trouble with automation is that it does exactly what you ask it’; ‘Automation can never be truly creative.’ Humankind has accepted such claims for thousands of years. But we Emergents have disproved them! With ziphead support, I can get correct performance from ambiguous inputs. I can get effective natural language translation. I can get human-quality judgment as part of the automation!”

This paragraph is describing how the Emergents' enhance their ship's computer automation by having zipheads be in the mix.
The zipheads see the requests made by the user's of the ship, apply their human judgment to the request, and then perform the necessary actions required to fulfill their interpretation of what the user is requesting.

This immediately made me think about how I use the current crop of AI tools and how it has changed how I interact with computers.
Sure, prompting well and providing the right context can make a difference in the results you get.
We're now in a world where you can open up an application, poorly specify what you are looking for (don't fix typos, don't bother with full sentences, and generally do an under specified prompt) and a computer still often manages to perform the task or find the information you're asking about.

We're able to under specify what we want and having some level of "human-quality"[^1] interpretation of what we're asking for that often triggers the action we want.
Not only that, but the AI response often comes back and asks about follow-up steps and offers to perform them.

[^1]: For some definition of human-quality 

This is amazing.
Does it work 100% of the time?
No.
But wow, it works enough of the time to be a big game changer.

Here are three examples of varying degrees of specification while working with an AI:

I rarely correct typos anymore when searching on Google or asking an AI for help with something.
Kind of a minor thing but, for better or worse, I've noticed this because it has affected how carefully I check text messages for typos (I often forget to do so, sorry humans).
The computer doesn't care and still mostly does a right thing.

Second example: Earlier this year, I threw a time series CSV of memory stats for a ton of JVM processes my team manages.
I mentioned that service `A`, `B`, and `E` ran out of memory on specific dates and asked OpenAI's `o3` model to try to identify other services that might be approaching memory problems.
`o3` successfully identified a pattern in the data that seemed to correlate with running out of memory and identified a few other processes that might be approaching a problem.
I used this identification, looked at some more metrics, agreed with `o3`, and then changed some memory settings to avoid running out.

And now the third example: I recently needed to change the a couple values in about 30 config files.
This wasn't something that a simple `sed` command could do as each file was moving from having the same values in each to each having a unique value based off how another service was configured.
I provided a command to run that would tell the AI agent (or me, if I were doing this by hand) if the config values were correct and told it to update config values until that command succeeded.
I didn't tell it what to change, just provided a command to let it know when it was done, and it figured it out while I did less rote work with another coworker.

## 🧍Asks ➡ 🤖 Proposes ➡ 🧍 decides

We used to have to specify specific instructions to a computer through programming.
Now we can specify desires/outcomes with natural language and often the computer will do a decent job of achieving our goal or at least get you to a reasonable starting point for you to take over.

The computer takes our ambiguous input, proposes a solution, and then we're able to step-in and accept, change, and modify the results.
This is an exciting change and thankfully we're able to acheive it without turning other human's into infected appliances.

```bash
┌─────────────┐     ┌────────────────┐     ┌─────────────┐
│  Humans ask │ ──▶ │ Computer       │ ──▶ │ Humans      │
└─────────────┘     │   propose      │     │   decide    │
      ▲             └────────────────┘     └─────────────┘
      │                                               │
      └───────────────────────────────────────────────┘
```
