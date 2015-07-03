---
layout: post
title: "GitHub Code Reviews: How-to comment on every line"
date: 2015-07-03 09:40:48 -0500
comments: true
published: false
categories: 
- code review
- process
- github
---

Late last year I wrote about the
[effective code review process](http://jakemccrary.com/blog/2014/12/09/an-effective-code-review-process/)
I started at Outpace. The process works well; participants say
its the most effective review process they've experienced. If you
haven't read the above post you should but below is an abbreviated
list of steps.

#### Steps for GitHub code review

1. Select the code to review. Usually this is an entire project.
1. About a week before the review, create a branch and delete all the
   code under review.
1. Push branch to GitHub and open a pull request. The pull request
   provides a location where comments can be made on every line of code.
1. Schedule a code review meeting in the future. Make sure
   participants have two to three days to asynchronously review the
   code in the pull request.
1. Have the code review. Get everyone together (video chat, in person)
   and go through the comments on the pull request and discuss. Add
   action items as a comment. The leader of the code review keeps
   dicussion moving.

Its a lightweight process. If you're already using GitHub it
doesn't bring in any other tools. The GitHub pull request interface
has good performance.

One complaint about this process is that the code you're reviewing
appears as deleted in the pull request. It is a superficial complaint
but I'll admit seeing the entire codebase as deleted can feel a bit
weird. 

For the most recent code review I setup I figured out a way to
have all the code appear as added. Here are the steps:

1. `git checkout -b empty-repo`
1. `rm -rf /tmp/repo && mkdir /tmp/repo && cp -R * /tmp/repo`
1. `rm -rf *`
1. `git commit -am 'remove all files'`
1. `git push -u origin empty-repo`
1. `git checkout -b code-review`
1. `cp -R /tmp/repo/* .`
1. `git add` the files you want to review.
1. `git commit -m 'adding files for review'`
1. `git push -u origin code-review`
1. Go to project on GitHub and switch to the `code-review` branch.
1. Open a pull request comparing the `empty-repo` and the
   `code-review` branch. 

Voila, you now have a pull request with every line under review marked
as added instead of deleted!

