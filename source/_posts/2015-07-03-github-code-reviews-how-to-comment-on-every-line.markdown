---
layout: post
title: "GitHub Code Reviews"
date: 2015-07-03 09:40:48 -0500
comments: true
published: false
categories: 
- code review
- process
- github
---

Last December I wrote about the
[effective code review process](http://jakemccrary.com/blog/2014/12/09/an-effective-code-review-process/)
I started at Outpace. The process works well; participants say it is
the most effective review process they've experienced. The rest of
this post is a summary of the process with a bit of an enhancement
around setting up the code for review. I'd recommend you read the
original
[post](http://jakemccrary.com/blog/2014/12/09/an-effective-code-review-process/)
for a bit more color on the process.

#### Steps for GitHub code review

1. Select the code to review.
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
   discussion moving.

Its a lightweight process. If you're already using GitHub it doesn't
bring in any other tools and, unlike some dedicated code review
software I've used, the GitHub pull request interface has good
performance.

One complaint about this process is that the code you're reviewing
appears as deleted in the pull request. It is a superficial complaint
but I'll admit seeing the entire code base as deleted can feel a bit
weird. 

For the most recent code review, I figured out a way to have all the
code appear as added. Here are the commands I just ran while writing
this post along.

``` console

# cd to the repository you are reviewing.
cd blog

# Make a new branch.
git checkout -b empty-repo

# Copy all files in repo to a temporary directory.
rm -rf /tmp/repo && mkdir /tmp/repo && cp -R * /tmp/repo

# Remove all files from repository, commit, and push to GitHub.
rm -rf *
git commit -am 'remove all files'
git push origin empty-repo

# Create a new branch with the empty-repo as the parent.
git checkout -b code-review

# Copy back in the files and add the files you want to review.
# Commit and push to GitHub.
cp -R /tmp/repo/* .
git add files-to-review
git commit -m 'adding files for review'
git push origin code-review

# Now, go to project on GitHub and switch to the code-review branch.
# Open a pull request comparing the empty-repo and the code-review
# branch.

```

Voila, you now have a pull request with every line under review marked
as added instead of deleted! It takes a little more than two times the
number steps required to open a pull request with the code under
review deleted but you might find it worth it. Seeing code as added
instead of removed is a minor thing but for some developers it makes a
difference. It is nice to know it is possible.
