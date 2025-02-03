---
dated-url: true
layout: post
title: "Emacs: Capture shell command output in temporary buffer"
date: 2013-08-10 14:33
comments: true
categories: [emacs, elisp]
---

My text editor of choice is Emacs. Its extensibility is a major
contributor to this preference. The ease of adding additional
functionality means you can customize it to your liking. You should
not go overboard and change too much of the default behavior but you
should feel free to add additional features.

I recently found myself often editing a file in emacs and then
switching to a terminal and running a bash script to see how the
output changed. This is part of my work flow for shutting down or
starting new server processes. Since this is something I'll be doing
quite frequently in the future, I wrote some Emacs Lisp to run the
shell script and display the output in a temporary buffer. With this
function in place I no longer have to toggle to a terminal and run a
command.

I'm picky and I wanted this output buffer to have the same behavior as
the help buffer. That is, I wanted to be able to close the buffer by
just hitting the letter `q`. It took me a while to figure out how to
do this so I thought I would share it here in hopes it might benefit others.

First I'll show the code and then I'll explain what it is doing.

```lisp
(defun blog-example ()
  (interactive)
  (with-output-to-temp-buffer "*blog-example*"
    (shell-command "echo This is an example"
                   "*blog-example*"
                   "*Messages*")
    (pop-to-buffer "*blog-example*")))
```

The above snippet defines a function named `blog-example`. It takes no
arguments and is interactive (as indicated by the second line calling
`interactive`). This call to `interactive` makes `blog-example`
available to be called interactively, meaning you can call it after
triggering `M-x`. This is probably a simplification of what is
actually does, so if you care the documentation is available
[here](http://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html).

After the call to `interactive` we hit the core of this function, the
call to `with-output-to-temp-buffer`. This function a buffer name as a first argument
and additional forms. The output of those forms is put into the named
buffer.

The form I'm passing to `with-output-to-temp-buffer` is a call to
`shell-command`. `shell-command` will run `echo This is an example`
synchronously and redirect stdout to `*blog-example*` and stderr to
`*Messages*`.

The final line opens the buffer and switches focus to it. Now you can
look at the output and when you are ready to return just hit `q`.

This is a simplified example but it shows how easy it is to extend
Emacs functionality. Doing something similar to this made a task I do
frequently more pleasant.

My use case is a bit more complicated and involves saving the buffer
I'm currently editing and then running a command against the saved
file. Below is some sample code that does something similar.

```lisp
(defun example2 ()
  (interactive)
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save file %s? " (buffer-file-name))))
    (save-buffer))
  (with-output-to-temp-buffer "*blog-example*"
    (shell-command (concat "wc -l"
                           " "
                           (expand-file-name (buffer-file-name)))
                   "*blog-example*"
                   "*Messages*")
    (pop-to-buffer "*blog-example*")))
```
