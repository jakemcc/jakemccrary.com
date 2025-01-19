---
layout: post
title: Use git pre-commit hooks to stop unwanted commits
date: 2015-05-31 11:51
comments: true
published: true
categories:
- git
---

Sometimes you'll make a change to some code and not want to commit it.
You probably add a comment to the code and hope you'll either see the
comment in the diff before committing or just remember not to check in
the change. If you've ever done this you've probably also committed
something you didn't mean to commit. I know I have.

Luckily we can do better. Using git pre-commit
[hooks](https://git-scm.com/docs/githooks) we can make git stop us
from committing. Below is a git pre-commit hook that searches for the
text _nocommit_ and if found rejects the commit. With it you can
stick _nocommit_ in a comment next to the change you don't want
committed and know that it won't be committed.

### The code

```bash
#!/bin/sh

# If you use a GUI for controlling git, you might want to comment out the `tput` commands.
# Some users have had problems with those commands and whatever GUI they are using.

if git rev-parse --verify HEAD >/dev/null 2>&1
then
    against=HEAD
else
    # Initial commit: diff against an empty tree object
    against=$(git hash-object -t tree /dev/null)
fi

patch_filename=$(mktemp -t commit_hook_changes.XXXXXXX)
git diff --exit-code --binary --ignore-submodules --no-color > "$patch_filename"
has_unstaged_changes=$?

if [ $has_unstaged_changes -ne 0 ]; then
    # Unstaged changes have been found
    if [ ! -f "$patch_filename" ]; then
        echo "Failed to create a patch file"
        exit 1
    else
        echo "Stashing unstaged changes in $patch_filename."
        git checkout -- .
    fi
fi

quit() {
    if [ $has_unstaged_changes -ne 0 ]; then
        git apply "$patch_filename"
        if [ $? -ne 0 ]; then
            git checkout -- .
            git apply --whitespace=nowarn --ignore-whitespace "$patch_filename"
        fi
    fi

    exit $1
}


# Redirect output to stderr.
exec 1>&2

files_with_nocommit=$(git diff --cached --name-only --diff-filter=ACM $against | xargs -I{} grep -i "nocommit" -l {} | tr '\n' ' ')

if [ "x${files_with_nocommit}x" != "xx" ]; then
    tput setaf 1
    echo "File being committed with 'nocommit' in it:"
    IFS=$'\n'
    for f in $(git diff --cached --name-only --diff-filter=ACM $against | xargs -I{} grep -i "nocommit" -l {}); do
        echo $f
    done
    tput sgr0
    quit 1
fi

quit 0
```

Lines 3-10 figure out what revision to diff against. They can pretty
much be ignored.

Lines 11-30 are all about handling unstaged changes. They create a
patch with these changes and revert these changes from the repository.
Then, in the function `quit`, the unstaged changes are reapplied to
the repository. All of this is done so that _nocommit_ in a
un-committed piece of text doesn't cause the committed changes to be
rejected.

Some online guides suggest using `git stash` to achieve what is
described above. I started out using `git stash` but ran into problems
where I'd end up in weird states. Unfortunately I didn't take good
notes and I'm unable to describe the various bad things that happened.
Trust me when I say bad things did happen and that this way (create
patch, revert, apply patch) is much more successful.

Line 36 figures out what files contain _nocommit_. Lines 38-44 report
what files contain _nocommit_ and then rejects the commit by exiting
with a non-zero exit code. The first `tput` changes the output of the
`echo` commands to colored red and the second `tput` changes output
back to default.

> Warning: I know many developers that love using this and have had no problems. I get the occasional report of it not working. If it doesn't work, and it seems like you've lost changes, you can find the patch file wherever mktemp creates files on your local machine. I'd still recommend testing it out on a small changeset so if something doesn't work on your machine you don't have to both debug why and recreate your changes.

### Using with a single repository

To enable in a single repository you need to add the above code to a
`.git/hooks/pre-commit` file in your local repository and make that
file executable. Once you've done that try adding _nocommit_ to a file
and then try to commit it. The commit will be rejected if the
pre-commit hook is setup properly.

### Using with multiple repositories

I want this pre-commit hook enabled in all of my repositories. I use
git init templates to do this. `git help init` or a
Google search can help fill in the gaps with setting this up but below
are the steps I ended up taking.

1. `git config --global init.templatedir ~/.git-templates`
1. `mkdir -p ~/.git-templates/hooks`
1. `touch ~/.git-templates/hooks/pre-commit`
1. Copy and paste the above code into
   `~/.git-templates/hooks/pre-commit`
1. `chmod +x ~/.git-templates/hooks/pre-commit`

After following those steps any repository created by `git init` will
contain the pre-commit hook. To add to an existing repository `cd` into
the repo and run  `git init .`.

### Example output

If you try to commit some text with _nocommit_ in it you'll see
something similar to the image below and the commit will be rejected.

![Error message](/images/pre-commit-example.png)

If you ever need to commit and want to ignore pre-commit hooks
(example: If you are writing a blog post that is full of the text
_nocommit_) then you can ignore pre-commit hooks by using `git commit
--no-verify`.

I've found this pre-commit hook really useful. It has saved me from
committing numerous times. I'd recommend adopting it.


## Errata

*2015/12/23*

I'm updated the code to be more portable. It was brought to my
attention by a comment that the original code took advantage of some
bash extensions and specific `mktemp` behavior found in OS X. The
pre-commit code has now been tested works in OS X and Ubuntu 14.04.
There may be minor changes you need to perform to get it to work on
your system.

*2017/04/28*

Updated code to handle if `mktemp` fails and if whitespace changes
between creating a patch and applying it. Also adds in a change that
better handles whitespace in paths.
