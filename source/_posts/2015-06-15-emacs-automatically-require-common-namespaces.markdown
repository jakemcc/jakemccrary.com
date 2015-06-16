---
layout: post
title: "Emacs: automatically require common namespaces"
date: 2015-06-15 21:17:23 -0500
comments: true
published: false
categories: 
- emacs
- clojure
---

If you're writing Clojure in Emacs you should check out
[clj-refactor](https://github.com/clojure-emacs/clj-refactor.el). It
provides some pretty neat functionality including the ability to
extract functions, introduces `let` forms, and inline symbols. It also
has a feature called "magic requires" that will automatically require
common namespaces when you type their short form.

Out of the box five short forms are supported. They are `io` for
`clojure.java.io`, `set` for `clojure.set`, `str` for
`clojure.string`, `walk` for `clojure.walk`, and `zip` for
`clojure.zip`. If you type `(str/` then a `[clojure.string :as str]`
will be added to your `ns` form. It is pretty awesome. This feature is
on by default with `clj-refactor.el` but you can turn it off by adding
`(setq cljr-magic-requires nil)` 

This feature is also extensible. You can add your own mappings of
short form to namespace. The following snippet of elisp adds mappings
for `maps`, `seqs`, and `string`.

```
(dolist (mapping '(("maps" . "outpace.util.maps")
                   ("seqs" . "outpace.util.seqs")
                   ("string" . "clojure.string")))
  (add-to-list 'cljr-magic-require-namespaces mapping t))
```

It doesn't take a lot of code but having it is awesome. If there are
namespaces you frequently require I highly recommend setting this up.
