---
layout: post
title: "Using Bash-Preexec for monitoring the runtime of your last command"
date: 2020-04-20 17:46:41 -0500
comments: true
published: false
description: PUT SUMMARY HERE 
keywords: 'csv, keywords, here'
categories: 
---

My article on [putting the runtime of your last command into your bash prompt](/blog/2015/05/03/put-the-last-commands-run-time-in-your-bash-prompt/) is one of my most surfaced-by-google articles.
I highly recommend adding the previous commands runtime to your prompt.
This is still one of my favorite little tricks.

Why is this a great addition? To quote my previous article:

> I’m fairly certain the following scenario has happened to every terminal user. You run a command and, while it is running, realize you should have prefixed it time. You momentarily struggle with the thought of killing the command and rerunning it with time. You decide not to and the command finishes without you knowing how long it took. You debate running it again.

> For the last year I’ve lived in a world without this problem. Upon completion, a command’s approximate run time is displayed in my prompt. It is awesome.

I've been living without the above problem since sometime in 2014 and not having that problem is still awesome.

I recently switched to using [Bash-Preexec](https://github.com/rcaloras/bash-preexec) instead of directly using `trap` and `$PROMPT_COMMAND` for calling functions to start and stop tracking time.
Bash-Preexec lets you trigger a function (or multiple) right after a command has been read and right before each prompt.

The usage is pretty straight forward.
In the most basic case, you source `bash-preexec.sh` and then provide functions named `preexec`, invoked right before a command is executed, or `precmd`, invoked just before each prompt.
`bash-preexec.sh` can be downloaded from [the repo](https://github.com/rcaloras/bash-preexec/).

Moving to Bash-Preexec was [pretty minimal](https://github.com/jakemcc/dotfiles/commit/46fc3dc9d4d7d0d73152c77b7383645af42b3d5d).
Since I first published my original article I've also started using a script, [format-duration](https://github.com/jakemcc/dotfiles/blob/9c8c0315f35b55df6cef7e21261e3dcbbfac86e1/home/.bin/format-duration#L3-L4) by [Gary Fredericks](https://twitter.com/gfredericks_), to humanly format the time.
Below is a simplified snippet from my `.bashrc`.

```bash
function preexec {
  if [ "UNSET" == "${timer}" ]; then
    timer=$SECONDS
  else 
    timer=${timer:-$SECONDS}
  fi 
}

function precmd {
  if [ "UNSET" == "${timer}" ]; then
     timer_show="0s"
  else 
    the_seconds=$((SECONDS - timer))
    # use format-duration to make time more human readable
    timer_show="$(format-duration seconds $the_seconds)" 
  fi
  timer="UNSET"
}

# Add $last_show to the prompt.
PS1='[last: ${timer_show}s][\w]$ '

# a bunch more lines until the end of my .bashrc
# where I include .bash-preexec.sh
[ -f "$HOME/.bash-preexec.sh" ] && source "$HOME/.bash-preexec.sh"
```

Using Bash-Preexec cleaned up my `.bashrc` significantly.
