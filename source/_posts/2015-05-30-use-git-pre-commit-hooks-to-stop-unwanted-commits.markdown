---
layout: post
title: "Use git pre-commit hooks to stop unwanted commits"
date: 2015-05-30 13:23:47 -0500
comments: true
published: false
categories: 
- git
---

It isn't a great habit to get into but sometimes you'll make a change
to your code and not want to commit it. The most common way I've seen
this done is by adding some obnoxious comment and paying close
attention to diffs when committing. This relies on human memory and
requires human effort. Those are two things that are often unreliable.

Luckily we can do better. We can make the computer stop us from
committing. Using git pre-commit
[hooks](https://git-scm.com/docs/githooks) you can have git reject
commits. Below is a git pre-commit hook that looks for the text
"nocommit" and rejects any commit where that text is found. This means
you can stick _nocommit_ in a comment in whatever language you're
working in and that line will not be allowed to be committed.

### Code and explanation

Lines 3-10 figure out what revision to diff against. It can pretty
much be ignored.

Lines 11-30 of the example below are handling unstaged changes. This
is done so that partial commits without _nocommit_ can still be done
even if _nocommit_ exists in part of a file that isn't being
committed. This is the trickiest part of the pre-commit hook. Many
guides online say to use `git stash` for doing this but I've ran into
problems with that. Generating a patch, reverting changes, and
reapplying the patch has been worked better for me. With `git stash` I
found myself ending up in odd states. I wish I had taken better notes
when I was having problems with `git stash` but unfortunately I only
have my memory to go on and I'm unable to remember the specific
problems I was having. The problems were large enough that I spent a
considerable amount of time figuring out this patch approach.

Line 36 figures out what files being committed have _nocommit_ in
them. Lines 38-44 are reporting what files are causing the commit to
be rejected and rejects the commit (by exiting with a non-zero exit
code). The first `tput` changes the output of the `echo` commands to
have be red and the second `tput` changes output back to default.


```bash
#!/bin/sh

if git rev-parse --verify HEAD >/dev/null 2>&1
then
    against=HEAD
else
    # Initial commit: diff against an empty tree object
    against=$(git hash-object -t tree /dev/null)
fi

patch_filename=$(mktemp -t commit_hook_changes)
git diff --exit-code --binary --ignore-submodules --no-color > $patch_filename
has_unstaged_changes=$?

if [[ $has_unstaged_changes != 0 ]]; then
    echo "Stashing unstaged changes in $patch_filename."
    git checkout -- .
fi

function quit {
    if [[ $has_unstaged_changes != 0 ]]; then
        git apply $patch_filename
        if [[ $? != 0 ]]; then
            git checkout -- .
            git apply $patch_filename
        fi
    fi

    exit $1
}


# Redirect output to stderr.
exec 1>&2

files_with_nocommit=$(git diff --cached --name-only --diff-filter=ACM $against | xargs grep -i "nocommit" -l | tr '\n' ' ')

if [[ "x${files_with_nocommit}x" != "xx" ]]; then
    tput setaf 1
    echo "File being committed with 'nocommit' in it:"
    echo $files_with_nocommit | tr ' ' '\n'
    tput sgr0
    quit 1
fi

quit 0
```

### Using with a single repository

To enable this pre-commit hook in a single git repository you need
create (or modify) a `.git/hooks/pre-commit` file in your local git
repository to contain the above code. Once you've done that I'd
recommend testing it out by adding _nocommit_ to a file and trying to
commit it. If the pre-commit hook is setup properly than it should be
rejected.

### Using with multiple repositories

I've found the easiest way of getting this pre-commit hook in all of
my repositories is to modify or create a git template. `git help init`
or a Google search can help fill in the gaps with setting this up but
below are the steps I ended up taking.

1. Modify my `~/.gitconfig` to set `init.templatedir =
   ~/.git-templates`
1. `mkdir -p ~/.git-templates/hooks`
1. `touch ~/.git-templates/hooks/pre-commit`
1. Copy and paste the above code into
   `~/.git-templates/hooks/pre-commit`
1. `chmod +x ~/.git-templates/hooks/pre-commit`

After following those steps any repository created by `git init` will
contain the pre-commit hook. To add the pre-commit hook to an existing
repository you can run `git init .` in the repository's directory.

# End

I've found this pre-commit hook really useful. I try not to make
changes that I don't want committed but sometimes I've found its the
right thing to do. With this pre-commit hook I don't need to worry
about accidently committing.
