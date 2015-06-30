---
layout: post
title: "My favorite clj-refactor features"
date: 2015-06-30 07:57:59 -0500
comments: true
published: false
categories: 
- clojure
- emacs
---

If you write Clojure using Emacs you should check out
[clj-refactor](https://github.com/clojure-emacs/clj-refactor.el). It
is working better than ever and makes developing Clojure more
enjoyable.

I don't use all the features in clj-refactor. This is a combination of
there being many refactorings I don't find myself doing as well as
there are many I'm just can't remember or am not aware of. Below are
the features I use consistently. 

My favorite feature of clj-refactor is the
[magic requires](http://jakemccrary.com/blog/2015/06/18/emacs-automatically-require-common-namespaces/).
This feature lets you type a prefix (such as `(str/)`) and have the
namespace automatically added to your `ns` form (in this example
`[clojure.string :as str]`). It is awesome. You can also add
[your own]((http://jakemccrary.com/blog/2015/06/18/emacs-automatically-require-common-namespaces/)
prefix mappings.

My other most frequently used refactorings are
[introduce let](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-introduce-let),
[expand let](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-expand-let),
and
[move to let](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-move-to-let).
These three are very complementary and are a quick way if introducing
named locals.

[Add missing libspec](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-missing-libspec)
is a recent discovery of mine. Have you ever paired with a developer
who uses Intellij with Cursive and been a bit jealous of the
auto-requiring? I have. This refactoring lets you do that. Type
whatever symbol you want and clj-refactor tries to resolve it and then
require the containing namespace with correct prefix. Recently I broke
a massive namespace into a few smaller ones and this refactoring saved
me a ton of time.

I used to use
[move form](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-move-form)
when trying to reorganize namespaces but now I pretty much just cut
and paste and use
[add missing libspec](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-missing-libspec)
to fix the requires. I want to use __move form__ but I haven't had a
ton of success with it. __Add missing libspec__ plus cut and paste is
a few more steps but my success rate has been much higher.

[Sort ns](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-sort-ns)
does exactly what it says, it sorts your `ns` form. Once you get used
to keeping your `ns` forms sorted you won't go back.

[Extract function](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-function)
is another refactoring I recently stumbled upon. I've used it a few
times since then and when it works it is pretty awesome. I've had
unexpected behavior a couple of times but it was unclear if that was
my fault or it not handling macros well. If you're extracting a
function you might as well give it a shot.

The final feature is the
[automatic insertion of namespace declarations](https://github.com/clojure-emacs/clj-refactor.el/wiki#automatic-insertion-of-namespace-declaration)
when you create a new Clojure file. I nearly forgot to highlight this
feature because it requires no action on my side and it is amazing. If
I never have to type a namespace symbol again I'll be happy.

### Customization

Below is my entire clj-refactor setup from my Emacs init.el. It
doesn't take much to get it to a state I like.

```elisp
(require 'clj-refactor)

;; Add custom magic requires.
(dolist (mapping '(("maps" . "outpace.util.maps")
                   ("seqs" . "outpace.util.seqs")
                   ("times" . "outpace.util.times")
                   ("repl" . "outpace.util.repl")
                   ("time" . "clj-time.core")
                   ("string" . "clojure.string")))
  (add-to-list 'cljr-magic-require-namespaces mapping t))

(setq cljr-favor-prefix-notation nil)

(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (yas/minor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-x")))
```

If you use Emacs and write Clojure you should check out clj-refactor.
There are enough features that consistently work and help keep you in
the flow that it is worth using.
