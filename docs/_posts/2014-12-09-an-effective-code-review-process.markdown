---
layout: post
title: An Effective Code Review Process
date: 2014-12-09 20:44
comments: true
published: true
categories:
- process
- code review
- remote
---

[{% img center /images/justin-searls-code-reviews-harmful.png The way most organizations implement code reviews in teams is usually more harmful than helpful. I generally recommend going without them. - Justin Searls %}](https://twitter.com/searls/status/540603801955471360)

The above was tweeted [^1] recently and it resulted in some decent
discussion about code reviews. In the past six months at
[Outpace](http://www.outpace.com), I've been part of a handful of code
review sessions that have been extremely productive. After the reviews
many developers have expressed shock at the effectiveness of the process.
A tweet-sized overview of the process we've followed can be found in
[Carin Meier](https://twitter.com/gigasquid)'s
[responses](https://twitter.com/gigasquid/status/540606002547425281) to
the above tweet. Since you can't fit details into tweets the rest of
this post expands on our code review process.

[^1]: Reading through the discussion on Twitter after this tweet can give some hints as to what it takes to have an effective code review.

Some background before we dive into the details.
[Outpace](http://www.outpace.com) is a software company that
practices, despite every programmer working remotely, nearly 100% pair
programming. In addition, the team Carin and I are on do most of our
work through GitHub pull requests. Before merging with master, the
pull requests are reviewed by other teammates. Between pairing and
pull requests many eyes see every line of code as changes are made.

Even with all this, we've found value in having more traditional code
reviews. We've found that different feedback and action items emerge
from reviewing code that we already have than from reviews of code
changes (e.g., pull requests).

In addition to working for the team described above, the process below
has been successfully used to review an internal library where the
reviewers where mostly interested users with a couple contributors. It
has also been successful on teams that were not adherent to doing work
through reviewed pull requests.

### The Code Review Process

#### Step 1: Select the code to review

Typically we do this between a week and two weeks before the code
review. Here we identify the code we want to review and create a
two-hour meeting on a Friday at the end of day.

Having the meeting late on Friday helps create a relaxed environment.
The review becomes a time to unwind, enjoy a beverage of choice, and
talk about code. I haven't met a developer that doesn't enjoy
discussing how to make code better and this lets everyone finish the
week doing just that. The code review becomes an uplifting way to
finish a week.

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
view code that has changed. This process marks every line as deleted,
which causes every line to appear the _Files changed_ tab.

Opening the pull request a few days before the review meeting provides
a location for pre-meeting comments to be added. This lets reviewers
spend a couple days thinking about and commenting on the code.
Comments on the pull request indicate a conversation should happen
during the code review meeting.

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

We've seen impressive improvements to code quality in the projects
that have undergone this style of code review. Both small and large
changes have happened as a result. Code has become simpler, clearer,
and better understood. Additionally, the feeling of collective code
ownership has increased.

Teammates have been surprised at how well this process has worked.
More than a couple have said that historically they have not found
code reviews useful but that these were.

This style of code review has worked in a few different settings and I
encourage you to give it a shot.
