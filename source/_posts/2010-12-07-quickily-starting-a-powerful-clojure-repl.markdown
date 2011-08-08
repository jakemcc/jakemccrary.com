---
layout: post
title: Quickly starting a powerful Clojure REPL
categories: [clojure, emacs]
---

I often find myself browsing the Internet and then suddenly I want to have a Clojure REPL at my fingertips. As I've become better with emacs and [paredit](http://www.emacswiki.org/ParEdit) I've become dependent on the powerful editing this combo affords. The rest of this post details how I changed my five step process into a two step process. It does not explain basic emacs/slime setup but rather explains how I cut a few steps out of a suboptimal workflow for getting a powerful Clojure REPL up and running in emacs.

My previous workflow was the following:

1. Open a terminal
2. Change to the root of Clojure project where I use [Leiningen](https://github.com/technomancy/leiningen) and have swank-clojure as a dependency.
3. Run the command `lein swank`
4. Start emacs
5. Run `M-x slime-connect`

This five step process was terrible. From me seeing something interesting to try to having a REPL open took too much time.

Today I changed my process so it on takes two steps. They are:

1. Start emacs
2. Run `M-x clojure-swank`

This is a much better. I'll admit had a lot of room for improvement so it wasn't too hard to make it better. Below are the steps I took to cut three steps.

First, using Leiningen 1.4.0, I ran `lein install swank-clojure 1.3.0-SNAPSHOT`. This installed a script called swank-clojure into $HOME/.lein/bin. When run, this script starts a swank server waiting for connections on port 4005.

Next I wrote a function in [elisp](http://en.wikipedia.org/wiki/Emacs_Lisp) that gives emacs the ability to call the newly installed swank-clojure script, wait for the swank server to start, and then connect to it. This function, `clojure-swank`, can be seen below. It creates a buffer named `*clojure-swank*`, runs the newly installed script, and captures the output in the freshly created buffer. When the "Connection opened" line appears `slime-connect` is called, connecting emacs to the freshly started swank server. After this we are at the REPL with all the advantages that emacs and paredit give us.

``` cl
    (defun clojure-swank ()
      "Launch swank-clojure from users homedir/.lein/bin"
      (interactive)
      (let ((buffer (get-buffer-create "*clojure-swank*")))
        (flet ((display-buffer (buffer-or-name &optional not-this-window frame) nil))
              (bury-buffer buffer)
              (shell-command "~/.lein/bin/swank-clojure &" buffer))
        (set-process-filter (get-buffer-process buffer)
                            (lambda (process output)
                               (with-current-buffer "*clojure-swank*" (insert output))
                               (when (string-match "Connection opened on local port +\\([0-9]+\\)" output)
                                 (slime-connect "localhost" (match-string 1 output))
                                 (set-process-filter process nil))))
        (message "Starting swank.. ")))
```

I've also written a `clojure-kill-swank` function for stopping the swank server.

``` cl
    (defun clojure-kill-swank ()
      "Kill swank process started by lein swank."
      (interactive)
      (let ((process (get-buffer-process "*clojure-swank*")))
        (when process
          (ignore-errors (slime-quit-lisp))
          (let ((timeout 10))
            (while (and (> timeout 0)
                        (eql 'run (process-status process)))
              (sit-for 1)
              (decf timeout)))
          (ignore-errors (kill-buffer "*clojure-swank*")))))
```

Both of those functions need to be added to a location where they will get defined on emacs start-up. Once this is done the powerful REPL you are used to emacs providing can be at your finger tips in practically no time at all.

