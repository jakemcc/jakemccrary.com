---
dated-url: true
layout: post
title: Using Bash-Preexec for monitoring the runtime of your last command
date: 2020-04-21 18:22 -0500
comments: true
published: true
description: Put the runtime of your previous command in your terminal prompt using
  Bash-Preexec
keywords: bash, bash-preexec, time your last command
categories:
- bash
- linux
- osx
- command-line
---

My article on [putting the runtime of your last command into your bash prompt](/blog/2015/05/03/put-the-last-commands-run-time-in-your-bash-prompt/) is one of my most surfaced-by-google articles.
Why is this a great to your prompt? 
To quote my previous article:

> I’m fairly certain the following scenario has happened to every terminal user. You run a command and, while it is running, realize you should have prefixed it with `time`. You momentarily struggle with the thought of killing the command and rerunning it with `time`. You decide not to and the command finishes without you knowing how long it took. You debate running it again.

> For the last year I’ve lived in a world without this problem. Upon completion, a command’s approximate run time is displayed in my prompt. It is awesome.

I've been living without the above problem since sometime in 2014 and not having that problem is still awesome.

I have made some changes since 2014.

One change was switching to using [Bash-Preexec](https://github.com/rcaloras/bash-preexec) instead of directly using `trap` and `$PROMPT_COMMAND` for calling functions to start and stop tracking runtime.
Bash-Preexec lets you trigger a function (or multiple) right after a command has been read and right before each prompt.

The usage is pretty straight forward.
In the most basic case, you source `bash-preexec.sh` and then provide functions named `preexec`, which is invoked right before a command is executed, and/or `precmd`, which is invoked just before each prompt.
`bash-preexec.sh` can be downloaded from [its repo](https://github.com/rcaloras/bash-preexec/). 
The changes required to move to Bash-Preexec pretty [pretty minimal](https://github.com/jakemcc/dotfiles/commit/46fc3dc9d4d7d0d73152c77b7383645af42b3d5d).

The other change was introducing the script, [format-duration](https://github.com/jakemcc/dotfiles/blob/9c8c0315f35b55df6cef7e21261e3dcbbfac86e1/home/.bin/format-duration#L3-L4) by [Gary Fredericks](https://twitter.com/gfredericks_), to humanely format the time.
This script converts seconds into a more readable string (example: 310 to `5m10s`)

Here is a screenshot of everything in action (with a reduced prompt, my normal one includes git and other info).

{% img /images/runtime-humane-example.png 320 150 "Command line prompt showing runtimes of previous commands" "Command line prompt showing runtimes of previous commands" %}

Below is a simplified snippet from my `.bashrc` that provides runtimes using both of these additions.

```bash
preexec() {
  if [ "UNSET" == "${timer}" ]; then
    timer=$SECONDS
  else 
    timer=${timer:-$SECONDS}
  fi 
}

precmd() {
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
[[ -f "$HOME/.bash-preexec.sh" ]] && source "$HOME/.bash-preexec.sh"
```

No more wondering about the runtime of commands is great.
Introducing `format-duration` made reading the time easier while Bash-Preexec made reading the implementation easier.
I highly recommend setting up something similar for your shell.
