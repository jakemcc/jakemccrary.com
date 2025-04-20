---
layout: post
title: Setup Emacs to autoformat your Clojure code with Apheleia and zprint
date: 2025-04-20 13:36 -0500
comments: true
published: true
description: Learn how to configure Emacs with Apheleia and zprint to automatically format your Clojure code on save.
categories:
- emacs
- clojure
---

Keeping code consistently formatted is important for readability and maintainability.
Once you get used to having a computer format your code for you, manually formatting code can feel tedious.

For the last few years, my team has been using [zprint](https://github.com/kkinnear/zprint) to keep our Clojure codebase formatted to our specifications.
`zprint` is great because it runs extremely fast and is extremely customizable.
That customization can sometimes be a chore to get right but it enables you to rarely need to think about formatting.

I've recently migrated from my own custom `before-save-hook` that triggered `zprint` whenever I saved a buffer to using [Apheleia](https://github.com/radian-software/apheleia).
Apheleia is an Emacs package that applies code formatters automatically on save.

Here's the configuration I use in my Emacs setup:

```lisp
(use-package apheleia
  :straight (apheleia :host github :repo "radian-software/apheleia")
  :config
  (setf (alist-get 'zprint apheleia-formatters)
        '("zprint" "{:style [:community] :map {:comma? false}}" "--write" inplace))
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'zprint
        (alist-get 'clojure-ts-mode apheleia-mode-alist) 'zprint)
  (apheleia-global-mode t))
```

This snippet shows how to install and configure using [straight.el](https://github.com/radian-software/straight.el) and `use-package`.
The `:config` section instructs `apheleia` under what modes it should run `zprint` and how to run it.[^1]

[^1]: I don't actually use `:community` and have my own custom formatting configuration but am using `:community` in this post so the snippet is immediately useful to readers.

With this setup, your Clojure code will be automatically formatted using zprint every time you save.
No more manual formatting needed.
I've been running with this for a little while now and am enjoying it.
