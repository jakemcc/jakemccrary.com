---
layout: post
title: Speeding up magit
date: 2020-11-14 16:37 -0600
comments: true
published: true
description: Magit is great but on large repos it can be sluggish. Here is how you
  can speed it up.
keywords: emacs, magit
categories:
- emacs
---

[Magit](https://github.com/magit/magit) is a great Emacs tool and by far my favorite way of interacting with git repositories.
I use Magit nearly every day.

Unfortunately, refreshing the `magit-status` buffer is sluggish when you are working in a large repository.

A few months ago, I became sick of waiting and investigated how to speed up refreshing the status buffer.
After doing some research, I learned about the `magit-refresh-verbose` variable.

Setting `magit-refresh-verbose` to true causes Magit to print some very useful output to your `*Messages*` buffer.
This output shows how many seconds each step of `magit-status` takes.

Here is the output for the large repo that caused me to look into this.

```
Refreshing buffer ‘magit: example-repo’...
  magit-insert-error-header                          1e-06
  magit-insert-diff-filter-header                    2.3e-05
  magit-insert-head-branch-header                    0.026227
  magit-insert-upstream-branch-header                0.014285
  magit-insert-push-branch-header                    0.005662
  magit-insert-tags-header                           1.7119309999999999
  magit-insert-status-headers                        1.767466
  magit-insert-merge-log                             0.005947
  magit-insert-rebase-sequence                       0.000115
  magit-insert-am-sequence                           5.1e-05
  magit-insert-sequencer-sequence                    0.000105
  magit-insert-bisect-output                         5.3e-05
  magit-insert-bisect-rest                           1.1e-05
  magit-insert-bisect-log                            1e-05
  magit-insert-untracked-files                       0.259485
  magit-insert-unstaged-changes                      0.031528
  magit-insert-staged-changes                        0.017763
  magit-insert-stashes                               0.028514
  magit-insert-unpushed-to-pushremote                0.911193
  magit-insert-unpushed-to-upstream-or-recent        0.497709
  magit-insert-unpulled-from-pushremote              7.2e-05
  magit-insert-unpulled-from-upstream                0.446168
Refreshing buffer ‘magit: example-repo’...done (4.003s)
```

The total time is found in the last line and we can see it took four seconds.
Four seconds is an incredibly long time to wait before interacting with Magit.

You can change how much work Magit does by removing functions from the `magit-status-sections-hook` with `remove-hook`.
I looked at the timings and and tried removing anything I decided was slow and something I didn't think I'd miss.
For me, that list includes `magit-insert-tags-header`, `magit-insert-status-headers`, `magit-insert-unpushed-to-pushremote`, `magit-insert-unpushed-to-upstream-or-recent`, and `magit-insert-unpulled-from-upstream`. I also removed `magit-insert-unpulled-from-pushremote`.

You remove a function from a hook by adding elisp similar to `(remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)` to your Emacs configuration.

I use [use-package](https://github.com/jwiegley/use-package) to configure mine and below is what my `magit` section looks like.

Lines 20-25 remove the hooks.
I also hard-code `magit-git-executable` to be the full path of the `git` executable on line 5 because folks said this made a difference on macOS.

```elisp
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :custom
  (magit-git-executable "/usr/local/bin/git")
  :init
  (use-package with-editor :ensure t)

  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))
  :config
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))
```

After this change, my `magit-status` buffer refreshes in under half a second.

```
Refreshing buffer ‘magit: example-repo’...
  magit-insert-merge-log                             0.005771
  magit-insert-rebase-sequence                       0.000118
  magit-insert-am-sequence                           5.3e-05
  magit-insert-sequencer-sequence                    0.0001
  magit-insert-bisect-output                         5.5e-05
  magit-insert-bisect-rest                           1.1e-05
  magit-insert-bisect-log                            1.1e-05
  magit-insert-untracked-files                       0.247723
  magit-insert-unstaged-changes                      0.024989
  magit-insert-staged-changes                        0.018397
  magit-insert-stashes                               0.026055
Refreshing buffer ‘magit: example-repo’...done (0.348s)
```

What did I lose from the `magit-status` buffer as a result of these changes?
Here is screenshot of the original buffer.

![Buffer before changes](/images/magit-speed/magit-before.png)

And here is the buffer after.

![Buffer after changes](/images/magit-speed/magit-after.png)

The difference is drastic[^1].
And so is the speed difference.

[^1]: The before image is even missing some sections that would have gone missing in the after shot since I didn't want to put the effort.

The increased speed is worth losing the additional information.
I interact with `git` very often and much prefer using Magit to do so.
Before these changes, I found myself regressing to using `git` at the command line and I don't find that to be nearly as enjoyable.
Since I've made these changes, I'm back to doing 99% of my `git` interactions through Magit.

Don't settle for slow interactions with your computer.
Aggressively shorten your feedback cycles and you'll change how you interact with the machine.

#### Versions used when writing this article

This post was written with Magit version `20201111.1436` and Emacs `26.3` on macOS `10.15.7`.
I've been using these changes for a few months but do not remember or have a record of what Magit version I was using at the time I originally made these changes.

**edit on 2020/12/15**: I recently upgraded Emacs to tryout the native-comp work and can report this still works with with Emacs `28.0.50`, Magit `20201212.929`, and Git `2.29.2` running in macOS `11.0.1`.

**Warning**: This reduces the information Magit shows you. The status buffer will be blank if you have no changes. I find this tradeoff to be worth it.
