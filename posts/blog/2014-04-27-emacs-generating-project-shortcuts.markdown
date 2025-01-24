---
dated-url: true
layout: post
title: "Emacs: Generating project shortcuts"
date: 2014-04-27 16:38
comments: true
categories: [emacs, elisp]
---

I'm now writing Clojure nearly 100% of my time and as a result am spending more
time in Emacs. I'm working in a few different projects and
wanted a quicker way to jump between them. My first attempt at this
ended with me defining many functions that looked like the following.

``` cl
(defun b/test-refresh ()
  (interactive)
  (find-file "~/src/jakemcc/lein-test-refresh/test-refresh/project.clj"))
```

After writing a couple of these I decided the computer could do this
better than I could and decided to write some code to automate it. A
sample of my directory structure is shown below.

``` bash
jakemcc/
├── bookrobot
│   └── project.clj
└── lein-autoexpect
    └── project.clj
```

Taking advantage of this structure I wrote some Emacs lisp to walk a
directory and define functions that open up any found project.clj
files.

``` cl
;; -*- lexical-binding: t -*-

(defun open-file-fn (file) 
  (lambda ()
    (interactive)
    (find-file file)))

(defun create-project-shortcuts (prefix base)
  (dolist (elt (directory-files base))
    (let ((project (concat base "/" elt "/project.clj")))
      (when (file-exists-p project)
        (fset (intern (concat prefix elt)) (open-file-fn project))))))
```

`open-file-fn` creates an anonymous interactive function (meaning the function
can be called interactively) that opens `file`. It takes advantage of
the feature in Emacs 24 that enables lexical scoping by adding `;; -*-
lexical-binding: t -*-` to the top of your Emacs lisp file. This lets
the anonymous function capture `file`.

`create-project-shortcuts` takes in a `prefix` and a `base` directory.
It searches `base` for directories that contain a
project.clj file. For each found project.clj file a function is
created (using `fset`) with the name of the containing directory
prefixed by `prefix`.

With those two functions defined all that is left is to call
`create-project-shortcuts`.

``` cl
(create-project-shortcuts "b/" "~/src/jakemcc")
```

Now `b/bookrobot` and `b/lein-autoexpect` are available after hitting
`M-x`.

I've used this code to create quick
shortcuts to all of my work and non-work projects. It has been
immensely useful for jumping around projects.