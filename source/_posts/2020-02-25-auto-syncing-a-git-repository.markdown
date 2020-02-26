---
layout: post
title: "Auto-syncing a git repository"
date: 2020-02-25 18:50:10 -0600
comments: true
published: false
description: Here is a mostly automated way of syncing a directory using git whenever a file changes.
keywords: 'git, notes, bash, org-mode'
categories: 
- bash
- utilities
- tools
- osx
- linux
---

Off and on I've experimented with keeping notes in a plain text format.
Most recently this has been me keeping track of notes using [Org mode](https://orgmode.org/).

I keep my notes in a git repository in my home directory, `~/org/`.
I want those notes to be synced between my various machines.
Historically I've reached for something like Google drive or Dropbox to do this but this time I reached for git and GitHub.

Below is the script that I ended up cobbling together from various sources found online.
It works on my macOS and linux machines.

The important lines of the program are the lines in the last loop.
Whenever a file-watcher notices a change or 10 minutes passes, the loop pulls changes from a remote repository, commits any local changes, and pushes to the remote repository.

I keep this running in a background terminal and I check periodically to confirm it is still running.
I could do something fancier but this isn't a critical system and the overhead of checking every couple days is nearly zero.
Most of the time checking happens by accident when I accidentally maximize the terminal this runs this.

```bash
#!/bin/bash

set -e

TARGETDIR="$HOME/org/"

stderr () {
    echo "$1" >&2
}

is_command() {
    command -v "$1" &>/dev/null
}

if [ "$(uname)" != "Darwin" ]; then
    INW="inotifywait";
    EVENTS="close_write,move,delete,create";
    INCOMMAND="\"$INW\" -qr -e \"$EVENTS\" --exclude \"\.git\" \"$TARGETDIR\""
else # if Mac, use fswatch
    INW="fswatch";
    # default events specified via a mask, see
    # https://emcrisostomo.github.io/fswatch/doc/1.14.0/fswatch.html/Invoking-fswatch.html#Numeric-Event-Flags
    # default of 414 = MovedTo + MovedFrom + Renamed + Removed + Updated + Created
    #                = 256 + 128+ 16 + 8 + 4 + 2
    EVENTS="--event=414"
    INCOMMAND="\"$INW\" --recursive \"$EVENTS\" --exclude \"\.git\" --one-event \"$TARGETDIR\""
fi

for cmd in "git" "$INW" "timeout"; do
    # in OSX: `timeout` => brew install coreutils
    # in OSX: `fswatch` => brew install fswatch
    is_command "$cmd" || { stderr "Error: Required command '$cmd' not found"; exit 1; }
done

cd "$TARGETDIR"
echo "$INCOMMAND"

while true; do
    eval "timeout 600 $INCOMMAND" || true
    git pull
    sleep 5
    STATUS=$(git status -s)
    if [ -n "$STATUS" ]; then
        echo "$STATUS"
        echo "commit!"
        git add .
        git commit -m "autocommit"
        git push origin
    fi
done
```
