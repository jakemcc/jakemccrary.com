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
seems to be working better than ever and makes developing Clojure more
enjoyable.

I don't use all the features in clj-refactor. There are many
refactorings that I don't find myself doing. There are also many
refactorings that I'm not aware of. The rest of this post highlights
some of the ones I use enough to remember.

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
using Intellij with Cursive and been a bit jealous of the
auto-requiring? I'll admit I have. This refactoring lets you do that.
Type whatever symbol you want and this tries to resolve it and require
the needed namespace with correct prefix. This refactoring was
extremely useful when I was doing a large reorganization recently.

I used to use
[move form](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-move-form)
when trying to reorganize namespaces but now I pretty much just cut
and paste and use
[Add missing libspec](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-missing-libspec)
to fix up the requires. I want to use __move form__ but I just haven't
had a ton of success with it. Its a couple more steps but the success
rate has been much higher.

[Sort ns](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-sort-ns)
does exactly what it says, it sorts your `ns` form. Once you get used
to keeping your `ns` forms sorted you won't go back.

[Extract function](https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-function)
is another refactoring I recently stumbled upon. I've used it a few
times since then and when it works it is pretty awesome. I've had
unexpected behavior a couple of times but it was unclear if that was
my fault or it not handling macros well. If you're extracting a
function you might as well give it a shot.

The final feature that I almost forgot to list is the
[automatic insertion of namespace declaration](https://github.com/clojure-emacs/clj-refactor.el/wiki#automatic-insertion-of-namespace-declaration)
when you create a new Clojure file. I nearly forgot this because this
takes no action on my side and it is amazing. If I never have to type
a namespace symbol again I'll be happy.
