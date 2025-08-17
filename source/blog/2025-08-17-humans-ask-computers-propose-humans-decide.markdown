---
layout: post
title: "Humans ask, computers propose, humans decide"
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

**Warning: There are minor spoilers of parts of _A Deepness in the Sky_ in this article.**

I was reading Vernor Vinge's [_A Deepness in the Sky_](https://en.wikipedia.org/wiki/A_Deepness_in_the_Sky) when a paragraph made me think of today's AI tools.

In _A Deepness in the Sky_, one of the groups of humans, the Emergents, has figured out how to take advantage of a "mindrot" virus that was a plague on their homeland.
Once a person is infected, the Emergents are able to manipulate the mindrot to force an obsession.
This practically turns the infected person, colloquially called a ziphead, into a specialized appliance focused on their obsession and little else.

In the following paragraph, one of the Emergents talks about how they use a subset of the zipheads to enhance their ship's computer:

> They left the group room and started back down the central tower. “See, Pham, you—all you Qeng Ho—grew up wearing blinders. You just know certain things are impossible. I see the clichés in your literature: ‘Garbage input means garbage output’; ‘The trouble with automation is that it does exactly what you ask it’; ‘Automation can never be truly creative.’ Humankind has accepted such claims for thousands of years. But we Emergents have disproved them! With ziphead support, I can get correct performance from ambiguous inputs. I can get effective natural language translation. I can get human-quality judgment as part of the automation!”

The zipheads see the requests made by the users of the ship, apply their human judgment to the request, and then work with the computer to fulfill their interpretation of what the user is requesting.
This allows the Emergents to make ambiguous requests to their ship's computer, requests a human would understand but a computer could not, and get back quality results.
There are literally humans-in-the-loop of the Emergents' computer system.

This paragraph made me think about how I use the current crop of AI tools and how it's changed how I interact with computers.
I can now open an app and poorly specify what I want (don't fix typos, don't bother with full sentences, be vague) and the computer still often manages to perform the task or find the information I'm asking about.

I can underspecify what I'm looking for and get approximately a "human-quality"[^1] fulfillment of that request.
Not only that, but the AI response often comes back and asks about follow-up steps and offers to perform them.
And all without enslaving other humans with a virus and attaching them to the computer.

[^1]: For some definition of human-quality 

This is amazing.
Does it work 100% of the time?
No.
But wow, it works enough of the time to be a big game changer.

Here are three examples of varying degrees of specification while working with an AI:

### Typos barely matter

I rarely correct typos anymore when searching on Google or asking an AI for help with something.
The computer doesn't care and still mostly does the right thing.
To be fair, this has been gradually happening over time with Google's ability to make sense of garbage searches, but modern AI tools have drastically accelerated it.

### Time series triage with `o3`

Earlier this year, I threw a CSV of memory stats and usage metrics for a ton of JVM processes my team manages at OpenAI's `o3` model.
I mentioned three services that ran out of memory on specific dates and asked it to identify other services that might be approaching memory problems.
`o3` identified a pattern in the data that correlated with running out of memory and flagged a few other processes that might be approaching a problem.
I looked at more metrics, agreed with `o3`, and then changed some memory settings to avoid future problems.

### Config migrations

I recently needed to change a couple of values in about 45 config files.
This wasn't something a simple `sed` could do because each file needed a unique value derived from another service's config.
I provided a command to run that would tell the AI agent (or me, if I were doing this by hand) if the config values were correct and told it to run the command and fix the problems.
I didn't specify what to change; it figured it out while I worked with some coworkers on figuring out a bug.
Once I wrapped up with my coworkers, I reviewed the changes, agreed with them, and moved on to the next task.

## 🙂 Asks ➡ 🤖 Proposes ➡ 🙂 Decides

We used to have to specify specific instructions to a computer through programming.
Now we can specify desires/outcomes with natural language and often the computer will do a decent job of achieving our goal or at least getting us to a reasonable starting point to take over.

The computer takes our ambiguous input, proposes a solution, and then we're able to step in and accept, change, and modify the results.
This is an exciting change and thankfully we're able to achieve it without turning other humans into infected appliances.

```bash
┌─────────────┐     ┌────────────────┐     ┌─────────────┐
│  Humans ask │ ──▶ │ Computer       │ ──▶ │ Humans      │
└─────────────┘     │  proposes      │     │   decide    │
      ▲             └────────────────┘     └─────────────┘
      │                                               │
      └───────────────────────────────────────────────┘
```

This feels like Vinge's ziphead-supported computer but we've replaced infected humans with AI models.
