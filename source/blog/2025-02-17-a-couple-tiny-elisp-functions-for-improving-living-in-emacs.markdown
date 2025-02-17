---
layout: post
title: A couple tiny elisp functions for improving living in Emacs
date: 2025-02-17 12:27 -0600
comments: true
published: false
description: Elisp functions for quickly change font size and switch window split orientation
categories:
- emacs
- elisp
---

I've been using Emacs for nearly twenty years but have struggled to quickly knock out tiny eplisp functions for making my experience nicer.
Despite that, I've managed to write both small and medium sized helpers for enhancing my Emacs experience.
Now that LLMs exist and are reasonably good, it has lowered the bar for me modifying my Emacs environment with little helper functions.

These have only been tested and used in Emacs 29.4 on macOS.

## Quickly change font sizes

I find myself working on a variety of monitor sizes and resolutions.
This function lets me quickly switch between font sizes.

```lisp
(defun jm/choose-font-size ()
  "Choose between three different font sizes: 16, 18, and 20."
  (interactive)
  (set-face-attribute 'default nil :height
                      (* 10 (string-to-number
                             (completing-read "Choose font size: "
                                              (mapcar #'number-to-string '(16 18 20)))))))
```

## Change window split orientation

First some definitions from the Emacs manual.

> A frame is a screen object that contains one or more Emacs windows (see Windows). It is the kind of object called a “window” in the terminology of graphical environments; but we can’t call it a “window” here, because Emacs uses that word in a different way. 
> - [Emacs manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Frames.html)

> A window is an area of the screen that can be used to display a buffer (see Buffers). Windows are grouped into frames (see Frames). Each frame contains at least one window; the user can subdivide a frame into multiple, non-overlapping windows to view several buffers at once.
> - [Emacs manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Windows.html)

I primarily work with a single frame split into at most two windows.
I haven't found a setup that I like for working with more than two windows, so I try to avoid it.
I find myself often wanting to toggle between a vertical split and a horizontal split
A picture is worth a thousand words and a gif even more.
Below is a demo and code that enables this functionality.

![Demo of toggling window orientation](/images/toggle-window-orientation.gif) 

```lisp
(defun jm/toggle-window-split ()
  "Toggle between horizontal and vertical split for two windows. Thanks ChatGPT."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
```

