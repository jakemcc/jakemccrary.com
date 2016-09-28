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
---

Since February of this year I've had pretty awesome command line
history in my Bash shell. My ideal command history would let me
globally search the history of every shell yet when I hit the up arrow
would only cycle through my current shell's history. This is exactly
the setup I've been able to achieve This is possible because of a
utility called [hstr](https://github.com/dvorka/hstr).

## What is `hstr`?

hstr is a neat Bash and Zsh utility that lets you easily search, view,
and manage your command history. It provides an interface that lets
you search your history, view the results, and select a command. If
you want to see what it looks like
the [README](https://github.com/dvorka/hstr/blob/master/README.md) has
images and this [video](https://www.youtube.com/watch?v=sPF29NyXe2U)
provides a nice overview. If you are running OS X you can install it
using brew by running `brew install hh`.

## Making global history searchable but arrows cycle through local history

hstr is a neat tool but my favorite part of my setup is how
the global command history is searchable but only a shell's local
history is cycled through using the arrow keys. This is achieved by
some manipulation of environment variables and where history is written.

The first thing to add to your shell's setup is setting the
`$PROMPT_COMMAND` to append your shell's history to a global history
file. Below is found in my `.bashrc` file.

```
# Whenever a command is executed, write it to a global history
PROMPT_COMMAND="history -a ~/.bash_history.global; $PROMPT_COMMAND"
```

The next step is to bind a keystroke to run `hh` (which is what hstr
provides) with `$HISTFILE` pointing to `~/.bash_history.global`. I
wanted to fully replace the default command history searching (and I
use Emacs style keyboard shortcuts) so I've bound these actions to ctrl-r.

```bash
# On C-r set HISTFILE and run hh
bind -x '"\C-r": "HISTFILE=~/.bash_history.global hh"'
```

With those two additions to my `.bashrc` I've achieved my ideal
command history searching. I can switch between shells and search all
of my history and can cycle through a shell's local history with my
arrow keys.
