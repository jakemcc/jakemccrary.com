---
layout: post
title: "Emacs: Capture shell command output in temporary buffer"
date: 2013-08-10 14:33
comments: true
categories: [emacs, elisp]
---

My prefered text editor of choice is Emacs. The extensibility of it is
the reason for this preference. The ease of adding additional
functionality means you can customize it to your liking. I don't think
you should go overboard with changing too much default behavior but I
do think you should feel free to add additional features.

I recently added a function which runs a shell script I often run and captures
the output in a temporary buffer. The buffer has the same
behavior as the buffer that pops up when you pull up a help dialogue
(`C-h m`). The help buffers have a nifty feature where if they are
focused you can hit `q` to close them and bounce back to your previous
location. It took me a while to figure out how to do this so I thought
I would share it here where it might benefit others.

First I'll show the code and then I'll explain what it is doing.

````elisp
(defun blog-example ()
  (interactive)
  (with-output-to-temp-buffer "*blog-example*"
    (shell-command "echo This is an example"
                   "*blog-example*"
                   "*Messages*")
    (pop-to-buffer "*blog-example*")))
````

First the function is using `defun` to define a function named
`blog-example`. The second line `(interactive)` is making this a
function that is interactively-callable, that is a function that you
can call. Without it there you cannot call it after typing `M-x`. More
information about `interactive` can be found over at the Emacs Lisp
[documentation](http://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html).

After the call to `interactive` we hit the core of this function.
`with-output-to-temp-buffer` takes the buffer name as a first argument
and then some forms and puts the output of forms into the named
buffer. If the buffer doesn't exist it will make it.

The form I'm passing to `with-output-to-temp-buffer` uses the
synchronous `shell-command` to run `echo This is an example`. The
stdout of the command is being directed to the same buffer that
`with-output-to-temp-buffer` is creating and directs stderr to
`*Messages*`.

The final line is simply changing the focus to the newly created
buffer. This isn't necesarry (opening help doesn't do this) but I
prefer it. I want to run a command, see the output, and then hit `q`
and be immediately back to what I was doing.

This is clearly a simplified example but shows how easy it is to
define your own custom elisp functions that run a command and display
the output in a temporary buffer. The workflow I improved by doing
this I do fairly regularly and this makes it easier to do.

My use case is a bit more complicated and involves saving the buffer
I'm currently editing and then running a command against the saved
file. Below is some sample code for acheiving this.

````elisp
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
````
