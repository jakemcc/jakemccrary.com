---
dated-url: true
layout: post
title: GitHub Code Reviews
date: 2015-07-03 11:22 -0500
comments: true
published: true
categories:
- code review
- process
- github
---

Last December I wrote about the
[effective code review process](/blog/2014/12/09/an-effective-code-review-process/)
I started at Outpace. The process works well; participants say it is
the most effective review process they've experienced. The rest of
this post is a summary of the process with a bit of an enhancement
around setting up the code for review. I'd recommend you read the
original
[post](/blog/2014/12/09/an-effective-code-review-process/)
for a bit more color on the process.

### Steps for GitHub code review

1. Select the code to review.
1. About a week before the review, create a branch and delete the code
   you're reviewing.
1. Push this branch to GitHub and open a pull request. This pull
   request provides a location where comments can be made on every
   line of code.
1. Schedule the code review meeting. Make sure participants have two
   to three days to asynchronously review the code in the pull
   request.
1. Have the code review. Get everyone together (video chat or in person)
   and go through the comments on the pull request and discuss. Add
   action items as a comment. The leader of the code review keeps
   discussion moving.

It's a lightweight process. If you're already using GitHub it doesn't
bring in any other tools and, unlike some dedicated code review
software I've used, the GitHub pull request interface has good
performance.

One complaint about this process is that the code you're reviewing
appears as deleted in the pull request. It is a superficial complaint
but seeing the entire code base as deleted can feel a bit weird.

For the most recent code review, I figured out how to have all the
code appear as added. The snippet below contains the steps and example
commands.

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
number steps required to open a pull request with the code deleted but
you might find it worth it. Seeing code as added instead of removed is
a minor thing but minor things can make a process more enjoyable. It
is nice to know it is possible.

If you aren't doing code reviews or have found them useless in the
past, I recommend you try out this process. This post is the
abbreviated version but it gives you enough to get started. If you
haven't done one in this style before, I'd highly recommend reading
the longer [post](/blog/2014/12/09/an-effective-code-review-process/)
as it gives some details that I've left out here.
