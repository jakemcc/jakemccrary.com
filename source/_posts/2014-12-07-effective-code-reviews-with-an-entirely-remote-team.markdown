---
layout: post
title: "Effective Code Reviews With an Entirely Remote Team"
date: 2014-12-07 15:45
comments: true
published: false
categories: 
- process
- code review
---

I've now been working at [Outpace](http://www.outpace.com/) for over a
year. [Outpace](http://www.outpace.com/) is fairly unique in that our
software development team is entirely remote yet we still practice
nearly 100% pair programming. In the last six months the teams I've
been part of have introduced two other forms (pair programming counts
as one form) of code reviews; pull requests for normal changes and
,what most people think of code reviews, periodic "review what we
have" traditional code reviews. All three practices; pair programming,
pull requests, and traditional code reviews, have shown immense
vvalue.

### The Code Review Process

How do you run a code review with an entirely remote team? Our code
review process fits the following steps.

#### Step 1: Schedule time and pick what to review

Typically we do this about a week to two weeks before the code review.
Here we identify the code we want to review and send out a meeting
invite for the end of day on a Friday.

We do the code reviews late on the day Friday because it sets up that
time to be a time to unwind with the team. It allows attendees to
optionally enjoy a beverage of their choice and lets everyone go into
the weekend feeling like they did something to direct the code base
into a cleaner direction.

#### Step 2: Start the code review

Late in the day on the Tuesday before the Friday code review meeting
we start the discussion of the code under review. We do this by
opening a pull request that includes all of the code being reviewed.
Since [GitHub](https://github.com) only lets you open pull requests
when code changes we perform the following steps.

1. Create a branch.
1. Delete the code that is being reviewed.
1. Push the branch to GitHub.
1. Open a pull request.

Those steps leave you with a pull request that allows you to comment
on every line under review.

We've found opening this pull request early in the week to be very
valuable. The pull request provides a spot to add and respond to
comments and lets reviewers do this at their leisure over a few days.
Comments are an indicator that a discussion should happen during the
Friday meeting.

#### Step 3: The code review meeting

This is the meeting that happens late at the end of the week. We get
everyone into a video conference and someone volunteers to lead the
code review. Another role is that of the action tracker.

The leader's responsibility is to direct the code review and keep it
moving forward. To do this the leader shares their screen with the
video conference and scrolls through the **Files Changed** view of the
pull request, stopping whenever they come to a comment.

The comments are read (usually silently) and discussion happens. When
it seems like a conclusion has been reached someone (often the leader)
does a quick summary and the action tracker records the next steps.
The next steps can be added as a comment on the pull request. While
the action is recording the next steps the leader scrolls to the next
comment.

This continues until either time runs out or the group runs out of
things to discuss.


We use GitHub pull requests to gather comments and direct the
discussion. You cannot submit a pull request without changes so in
order to open up the

1. On Monday schedule a two hour block of time near the end of the day
   on Friday. Talk about what we want to review.
1. Late in the day on Tuesday make a branch and delete the code you
   are planning on reviewing and open a pull request against this branch.
1. Wednesday through Friday are open for people to comment on the
   above pull request.
1. At the end of the day Friday everyone gathers in a video
   conference and we discuss the code.



