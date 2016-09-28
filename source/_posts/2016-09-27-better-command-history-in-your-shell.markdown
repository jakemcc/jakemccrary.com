---
layout: post
title: "Better command history in your shell"
date: 2016-09-27 17:16:02 -0500
comments: true
published: false
description: Here is how to have better command history searching with minimal changes to your shell's configuration.
keywords: 'bash, command line, history, hh, hstr'
categories: 
- command-line
- tools
- utilities
- bash
---

My ideal command history would let me search the history of every
shell but when I hit the up arrow it would only cycle through my
current shell's history. In February, I was able to achieve this setup
in large part because of a utility
called [hstr](https://github.com/dvorka/hstr).

## What is `hstr`?

hstr is a neat Bash and Zsh utility that lets you easily search, view,
and manage your command history. hstr provides a tool named `hh` that
provides a text interface for manipulating your command
history. To see what it looks like check out
the [README](https://github.com/dvorka/hstr/blob/master/README.md) and
this [video](https://www.youtube.com/watch?v=sPF29NyXe2U) tutorial. If
you are running OS X and use Homebrew you can install it by running `brew
install hh`.

## Making global history searchable but arrows cycle through local history

hstr is a neat tool but my favorite part of my setup is how the global
command history is searchable but only a shell's local history is
cycled through with the arrow keys. This is achieved by manipulating
where history is written and tweaking some environment variables.

The first step is to change your `$PROMPT_COMMAND` to append your
shell's history to a global history file. Below is the snippet that
does this from my `.bashrc` file.

```
# Whenever a command is executed, write it to a global history
PROMPT_COMMAND="history -a ~/.bash_history.global; $PROMPT_COMMAND"
```

The next step is to bind a keystroke to run `hh`, which is what hstr
provides, with `$HISTFILE` pointing to `~/.bash_history.global`. I
wanted to fully replace the default command history searching (and I
use Emacs style keyboard shortcuts) so I've bound these actions to ctrl-r.

```bash
# On C-r set HISTFILE and run hh
bind -x '"\C-r": "HISTFILE=~/.bash_history.global hh"'
```

With those two additions to my `.bashrc` I've achieved my ideal
command history searching. When I hit ctrl-r I'm searching all of my
history and yet I only cycle through a shell's local history with the
arrow keys. This small addition[^1] made my command line productivity
higher.

[^1]: My setup was inspired by [this](https://unix.stackexchange.com/questions/200225/search-history-from-multiple-bash-session-only-when-ctrl-r-is-used-not-when-a) StackExchange post.
