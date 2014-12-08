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

[{% img center /images/justin-searls-code-reviews-harmful.png The way most organizations implement code reviews in teams is usually more harmful than helpful. I generally recommend going without them. - Justin Searls %}](https://twitter.com/searls/status/540603801955471360)

The above was tweeted [^1] recently and I'm not sure I can disagree
with it. I will say that at [Outpace](http://www.outpace.com) we've
figured out a process that does lead to effective code reviews.
[Carin Meier](https://twitter.com/gigasquid), one of my teammates,
[responded](https://twitter.com/gigasquid/status/540606002547425281)
to the tweet and gave a brief overview of our process. The rest of
this post gives more details about our process.

[^1]: Reading through the discussion on Twitter after this tweet can give some hints as to what it takes to have an effective code review.

Some background before we dive into the code review process. Outpace
is a software company that practices, despite every programmer working
remotely, nearly 100% pair programming. In addition, the team Carin
and I are on do most of our work through GitHub pull requests. Before
merging with master the pull requests are reviewed by other teammates.
Between pairing and pull requests many eyes see every line of code as
change happens.

Even with pairing and work being done through pull requests we've
found value in more traditional code reviews (where part or all of the
system is reviewed). Take the time to review what we have (a more
traditional code review) has resulted in different types of changes
than when review code changes as they happen (pull requests with new
functionality).

In addition to working for the team described above, the process below
has been successfully used to review an internal library where the
reviewers where mostly interested users with a couple contributors. It
has also been successful on teams that were not adherent to doing work
through reviewed pull requests.

### The Code Review Process

#### Step 1: Select code to review

Typically we do this between a week and two weeks before the code
review. Here we identify the code we want to review and create a two
hour meeting on a Friday at the end of day.

Doing the code reviews late on a Friday plays a critical part in the
code review process. It is a good time for unwinding before the
weekend. Code reviewers grab their beverage of choice and we hang out
in a video conference in a relaxed environment. I don't think I've met
a developer that doesn't like talking about how to make code better
and this end of week meeting allows us to finish the week feeling like
we were part of something that results in cleaner code. It is a great
way to end a week.

#### Step 2: Open the code review

A few days (typically late Tuesday or early Wednesday) before the
Friday code review meeting we start the review. We do this by opening
a [GitHub](https://github.com) pull request. The following steps will
create a pull request where you can comment every line of code being
reviewed.

1. Create a local branch.
1. Delete the code being reviewed and commit locally.
1. Push the branch to GitHub.
1. Open a pull request.

These steps are necessary because GitHub pull requests only let you
view code that has changed. This process marks every line as deleted
causing every line to appear the _Files changed_ tab.

Opening the pull request a few days before the end of week meeting
provides a location for pre-meeting comments to be added. This lets
reviewers spend a couple days thinking about and commenting on the
code. Comments on the pull request indicate a conversation should
happen during the code review meeting.

#### Step 3: The code review meeting

Its finally Friday and time to review the code as a group. Everyone
joins a video conference and someone volunteers to lead the code
review. At least one other person volunteers to be a note taker.

The leader directs the code review and keeps it moving forward. To do
this the leader shares their screen with the video conference and
scrolls through the _Files changed_ view of the pull request. When a
comment appears on screen the leader stops scrolling and discussion
starts.

The comments are read (often silently) and discussion happens. The
leader tries to recognize when a conclusion has been reached or when
further discussion, outside of the code review, needs to happen. When
a conclusion is reached someone (often the leader) states a quick
summary and a note taker records the next steps. The next steps are
added as additional comments in the comment thread being discussed. As
the next steps are recorded the leader moves on to the next comment.

This continues until either time runs out or the group runs out of
things to discuss.

After the code review a volunteer turns the next steps comments into
Trello cards and we take care of the code review items as part of our
usual work.

### Results

So far I've seen impressive improvements to the code quality of the
projects that have done this style of code review. Both small and
large changes have happened after taking the time to reflect on the
code under review. Code has become simpler, clearer, and improved
performance as a result of code reviews. The feeling of collective
code ownership and general knowledge of the code has also improved.

Teammates have been surprised as how well this style of code review
has worked. More than a couple have said that historically they
haven't found code reviews useful but they have found these code
reviews useful.

This style of code review has worked in a few different settings and I
encourage you to give it a shot.
