---
dated-url: true
layout: post
title: Put the last command's run time in your Bash prompt
date: 2015-05-03 20:37
comments: true
published: true
categories:
- bash
- linux
- osx
- command-line
---

> An updated version of this post can be found [here](/blog/2020/04/21/using-bash-preexec-for-monitoring-the-runtime-of-your-last-command/)

I'm fairly certain the following scenario has happened to every
terminal user. You run a command and, while it is running, realize you
should have prefixed it with [`time`](http://linux.die.net/man/1/time). You
momentarily struggle with the thought of killing the command and
rerunning it with `time`. You decide not to and the command finishes
without you knowing how long it took. You debate running it again.

For the last year I've lived in a world without this problem. Upon
completion, a command's approximate run time is displayed in my
prompt. It is awesome.

## Overview 

Most of the code below is from a post on
[Stack Overflow](http://stackoverflow.com/a/1862762/491871). It has
been slightly modified to support having multiple commands in your
`$PROMPT_COMMAND` variable. Below is a minimal snippet that could be
included in your `.bashrc`.

``` bash
function timer_start {
  timer=${timer:-$SECONDS}
}

function timer_stop {
  timer_show=$(($SECONDS - $timer))
  unset timer
}

trap 'timer_start' DEBUG

if [ "$PROMPT_COMMAND" == "" ]; then
  PROMPT_COMMAND="timer_stop"
else
  PROMPT_COMMAND="$PROMPT_COMMAND; timer_stop"
fi

PS1='[last: ${timer_show}s][\w]$ '
```

Modify your `.bashrc` to include the above and you'll have a prompt
that looks like the image below. It is a minimal prompt but it
includes the time spent on the last command. This is great. No more
wondering how long a command took.

![Example of prompt](/images/prompt-timings.png)

## The details

`timer_start` is a function that sets `timer` to be its current value
or, if `timer` is unset, sets it to the value of `$SECONDS`.
`$SECONDS` is a special variable that contains the number of seconds
since the shell was started. `timer_start` is invoked after every
simple command as a result of `trap 'timer_start' DEBUG`.

`timer_stop` calculates the difference between `$SECONDS` and `timer`
and stores it in `timer_show`. It also unsets `timer`. Next time
`timer_start` is invoked `timer` will be set to the current value of
`$SECONDS`. Because `timer_stop` is part of the `$PROMPT_COMMAND` it
is executed prior to the prompt being printed.

It is the interaction between `timer_start` and `timer_stop` that
captures the run time of commands. It is important that `timer_stop`
is the **last** command in the `$PROMPT_COMMAND`. If there are other
commands after it then those will be executed and their execution
might cause `timer_start` to be called. This results in you timing the
length of time between the prior and current prompts being printed.

## My prompt

My prompt is a bit more complicated. It shows the last exit code, last
run time, time of day, directory, and git information. The run time of
the last command is one of the more useful parts of my prompt. I
highly recommend you add it to yours.

![My prompt](/images/my-prompt.png)

## Errata

*2015/5/04*

[Gary Fredericks](https://twitter.com/gfredericks_) noticed that the
original code sample broke if you didn't already have something set as
your `$PROMPT_COMMAND`. I've updated the original snippet to reflect
his [changes](https://twitter.com/gfredericks_/status/595249998838800384).