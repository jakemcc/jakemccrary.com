---
dated-url: true
layout: post
title: "Releasing lein-autoexpect 1.0"
date: 2013-08-05 19:50
comments: true
categories: [clojure, testing]
---

Put together a new release of
[lein-autoexpect](https://github.com/jakemcc/lein-autoexpect) today.
lein-autoexpect is a plugin for [Leiningen](http://leiningen.org/)
that monitors your source directories for changes and then reloads
your code and runs your
[expectations](https://github.com/jaycfields/expectations). It reports
test output to the console and optionally sends notifications to
[Growl](http://growl.info/) (and Growl like notification tools).

To use lein-autoexpect, add `:plugins [[lein-autoexpect "1.0"]]` to
your either your project's `project.clj` or your global
`~/.lein/profiles.clj`. To use the plugin run `lein autoexpect`.
This will display the test results to the console. To also have
results reported using Growl run `lein autoexpect :growl`.

Release 1.0 of lein-autoexpect upgrades its dependency on
[org.clojure/tools.namespace](https://github.com/clojure/tools.namespace/)
to version 0.2.4. It also no longer crashes if there isn't a Growl
connection available.

If you haven't tried using expectations and lein-autoexpect I
encourage you to give it a try. Having my tests run automatically made
a huge positive difference on my development experience.