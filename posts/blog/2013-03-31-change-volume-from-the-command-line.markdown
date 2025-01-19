---
dated-url: true
layout: post
title: Change volume from the command line
date: 2013-03-31 16:17
comments: true
categories: [Linux, command-line]
---

On my Ubuntu desktop the volume at 100% is often too quiet. With Ubuntu's default window manager I could open up the graphical "Sound Preferences" and bump the volume to above 100%. After using [i3 window manager](http://i3wm.org/) for a while I found myself missing this and took the time to figure out how to do it from the command line.

Ubuntu uses [PulseAudio](http://www.freedesktop.org/wiki/Software/PulseAudio) to handle sound related tasks. The tool [pacmd](http://linux.die.net/man/1/pacmd) allows you to change PulseAudio settings, such as volume, on the fly. The command is `pacmd set-sink-volume <sink-index> <sink-volume>` where `<sink-index>` is an identifier for your output device and `<sink-volume>` is an integer greater than or equal to zero. Zero represents muted and 65536 represents 100% volume. `<sink-index>` is the index found in the output from the `pacmd list-sinks` for your output card. In my case it is 0.

The below script makes changing volume with `pacmd` straightforward. I'm using Perl convert a percentage into the proper units for the <sink-volume> argument. Using this script if you want to pull a [Spinal Tap](http://www.youtube.com/watch?v=EbVKWCpNFhY) and go above 100% you simply pass in a number greater than 100.

``` bash
    #!/bin/bash

    if [ "$1" == "" ]; then
      echo "Need to supply a percentage"
      exit 1
    fi

    vol=$(perl -e "print int(65536 * ($1 / 100))")
    echo "Setting volume to $1 ($vol)"
    pacmd set-sink-volume 0 $vol
```
