---
dated-url: true
layout: post
title: How to be automatically notified when long running processes finish
date: 2020-05-04 20:35 -0500
comments: true
published: true
description: I get distracted when running slow processes. Here is how I get notified
  when they are done.
keywords: bash, osx, command line
categories:
- bash
- osx
- command-line
---

Let me set the stage.
I kick off the compilation of a large Scala codebase.
This will take minutes to finish, so I switch to Slack and catch up on what coworkers have posted.
Someone posted an interesting link and I follow it to an article.
Fifteen minutes later, I notice the compilation finished twelve minutes ago.
I silently grumble at myself, disappointed that I didn't start the next step twelve minutes ago.

Has some variation of the above happened to you?

It doesn't happen to me anymore because now my computer tells me when any long running process finishes.
This might sound annoying but it is great.
I no longer feel guilty[^1] for dropping into Slack and can immediately get back to the task at hand as soon the process finishes.

[^1]: I still feel a little guilty as doing so will break any momentum/flow I had going on, but that flow was already broken by the slowness of the command.

I've done this by enhancing on my setup for showing the [runtime of the previous command in my prompt](/blog/2020/04/21/using-bash-preexec-for-monitoring-the-runtime-of-your-last-command/).
You don't have to read that article for the rest of this one to make sense, but you should because it shows you how to add a very useful feature to your prompt.

Below is the code that causes my computer to tell me when it finishes running commands that takes longer than 30 seconds.
It is found in my `~/.bashrc`.
An explanation follows the code snippet.

```bash
# Using https://github.com/rcaloras/bash-preexec
preexec() {
  _last_command=$1
  if [ "UNSET" == "${_timer}" ]; then
    _timer=$SECONDS
  else 
    _timer=${_timer:-$SECONDS}
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
  if [ "UNSET" == "${_timer}" ]; then
     timer_show="0s"
  else 
    elapsed_seconds=$((SECONDS - _timer))
    _maybe_speak ${elapsed_seconds}
    timer_show="$(format-duration seconds $elapsed_seconds)"
  fi
  _timer="UNSET"
}

# put at the bottom of my .bashrc
[[ -f "$HOME/.bash-preexec.sh" ]] && source "$HOME/.bash-preexec.sh"
```

[Bash-Preexec](https://github.com/rcaloras/bash-preexec) triggers the `preexec`, immediately before a command is execute, and `precmd` functions, immediately before the shell prompt reappears.
Those two functions are enough to figure out how much time has elapsed while a command ran.
You setup Bash-Preexec by downloading [bash-preexec.sh](https://github.com/rcaloras/bash-preexec/blob/master/bash-preexec.sh) and sourcing it in your `~/.bashrc`.

`preexec` is passed the command being ran and it captures it in `_last_command`.
It also captures the current number of seconds the shell has been running as `_timer`.

`precmd` uses the value in `_timer` to calculate the elapsed time in seconds and then calls the function `_maybe_speak` with this as an argument.
It also does the work required for showing the elapsed time in my prompt.

If the elapsed time is greater than 30 seconds then `_maybe_speak` uses `cut` to discard the arguments of captured command, leaving me with the command itself.
It then uses `say` to produce an audible alert of what command just finished.
I discard the arguments because otherwise the `say` command can go on for a long time.

`say` is a tool that ships with macOS.
I haven't gotten around to it yet but I'll need to use something else on my Linux machines.

You may have noticed that I run `say` in the background and in a subshell.
Running it in the background lets me continue interacting with my shell while `say` finishes executing and running it in a subshell prevents text from appearing in my shell when the background job finishes.

With this setup, I can kick off a slow compile or test run and not feel so bad about dropping into Slack or reading Reddit. It is wonderful and I'd recommend it (though, I'd more strongly recommend not having commands that take a while to run).