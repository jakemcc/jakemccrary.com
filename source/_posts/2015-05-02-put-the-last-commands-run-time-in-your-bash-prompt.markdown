---
layout: post
title: "Put the last command's run time in your Bash prompt"
date: 2015-05-02 16:15:04 -0500
comments: true
published: false
categories: 
- bash
---

I'm fairly certain the following scenario has happened to most
terminal users. You run a command and while its running realize you
wish you had prefixed it with [time](http://linux.die.net/man/1/time).
The command finally finishes and you really wish you knew how long it
took.

For the last year I've lived in a world where I don't have to think
about if I want to time how long a command takes; timing commands
happens automatically and the results get displayed in my prompt. It
is pretty great.

## Overview 

Most of the below code is taken straight from a post on
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

PROMPT_COMMAND="$PROMPT_COMMAND; timer_stop"

PS1='[last: ${timer_show}s][\w]$ '
```

Modifying your `.bashrc` to include the above will leave you with a
prompt that looks like the image below. Notice how it tells us how
long the previous command took in the prompt. While the below prompt
isn't too great, having the time of the last command in it is pretty
awesome.

![Example of prompt](/images/prompt-timings.png)

## The details

The above code setups a trap that runs `timer_start` after every
simple command. The function `timer_start` sets `timer` to be
`$SECONDS` isn't already set to a value. Because `timer_stop` is part
of your `$PROMPT_COMMAND` it will be executed prior to your prompt
displayed. `timer_stop` calculates the timer value and unsets `timer`,
which allows `timer` to be set back to `$SECONDS` next time a command
is executed. Finally `$PS1` is set to include `$timer_show` in the
prompt.

It is important that `timer_stop` be the **last** command in
`$PROMPT_COMMAND`. If it isn't than your timings will be screwed up.
`timer_start` will be triggered by the commands run after `timer_stop`
and it will incorrectly set the `timer` value too early. This results
in you measuring the time since your last prompt was displayed instead
of the run time of the previous command.

## Fin

My prompt is a bit more complicated. It shows the last exit code, last
run time, time of day, directory, and git information. I'd rank the
git information as my most useful part of my prompt but the time of
the last command is a close second. I highly recommend you add it to
yours.

![My prompt](/images/my-prompt.png)
