---
layout: post
title: "How to be alerted when a long running process finishes"
date: 2020-05-02 17:11:02 -0500
comments: true
published: false
description: I get distracted when running slow processes. Here is how I get notified when they are done.
keywords: 'bash, osx, command line
categories: 
- bash
- osx
- command-line
---

Unfortunately, I'm frequently running commands that take a long time to finish.
I can only eagerly wait for completion, thinking about my next move, so many times before I get distracted by Slack or the Internet.
Turning to Slack or the Internet makes the waiting less painful, but then I don't notice the command finishing resulting in delays before taking my next action.

I recently made it so that my computer uses `say` to tell me when a command finishes after takes longer than 30 seconds.
This allows me to squeeze in other activities while slow commands run and continue with my next step as soon as the command finishes.

This builds on my setup for showing the [runtime of the previous command in my prompt](/blog/2020/04/21/using-bash-preexec-for-monitoring-the-runtime-of-your-last-command/).

I'm using [Bash-Preexec](https://github.com/rcaloras/bash-preexec) to trigger the `preexec` and `precmd` functions.
Bash-Preexec triggers `preexec` immediately before executing a command and `precmd` immediately before the prompt reappears.
You setup Bash-Preexec by downloading [bash-preexec.sh](https://github.com/rcaloras/bash-preexec/blob/master/bash-preexec.sh) and sourcing it in your `~/.bashrc`.

Below is found in my `~/.bashrc`.

```bash
# Using https://github.com/rcaloras/bash-preexec
preexec() {
  _last_command=$1
  if [ "UNSET" == "${timer}" ]; then
    timer=$SECONDS
  else 
    timer=${timer:-$SECONDS}
  fi 
}

_maybe_speak() {
    local elapsed_seconds=$1
    if (( elapsed_seconds > 30 )); then
        local c
        c=$(echo "${_last_command}" | cut -d' ' -f1)
        ( say "finished ${c}" & )
    fi
}

precmd() {
  if [ "UNSET" == "${timer}" ]; then
     timer_show="0s"
  else 
    elapsed_seconds=$((SECONDS - timer))
    _maybe_speak ${elapsed_seconds}
    timer_show="$(format-duration seconds $elapsed_seconds)"
  fi
  timer="UNSET"
}

# put at the bottom of my .bashrc
[[ -f "$HOME/.bash-preexec.sh" ]] && source "$HOME/.bash-preexec.sh"
```

`preexec` is passed the command being ran and it captures it in `_last_command`.
It also captures the current number of seconds the shell has been running.

`precmd` calculates the elapsed time in seconds and then calls the function `_maybe_speak` with this as an argument.
It also does the work required for showing the elapsed time in my prompt.

If the elapsed time is greater than 30 seconds then `_maybe_speak` uses `cut` to discard the arguments of captured command and then spawns a subshell to run `say` to give me an audible alert about what command just finished.
I discard the arguments because otherwise the `say` command can go on for a long time.

I run `say` in the background and in a subshell.
Running it in the background lets me continue interacting before `say` finishes executing.
Running it in a subshell prevents text from appearing in my shell when the background job finishes.

With this setup, I can kick off a slow compile or test run and not feel so bad about dropping into Slack or reading reddit.
