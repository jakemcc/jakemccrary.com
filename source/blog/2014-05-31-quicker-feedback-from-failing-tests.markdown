---
dated-url: true
layout: post
title: "Quicker Feedback From Failing Tests"
date: 2014-05-31 13:14
comments: true
categories: [clojure, testing]
---

Over the last couple of years I've desired quicker feedback from my Clojure tests.
This has resulted in the development of [lein-autoexpect](https://github.com/jakemcc/lein-autoexpect) and more recently [lein-test-refresh](https://github.com/jakemcc/test-refresh).
Each tool monitors your project for changes and on change uses [tools.namespace](https://github.com/clojure/tools.namespace) to reload your code and then reruns either your [expectations](http://jayfields.com/expectations/) or `clojure.test` tests.
Using tools like these has changed my development process.

Version 0.5.0 of **lein-test-refresh** was released last week.
This release enables even quicker feedback by tracking which tests fail and after reloading your code it runs those tests first.
Only when your previously failed tests pass does it then rerun all of your tests.

**lein-test-refresh** has had quite a few features added since I last wrote about it.
The [readme](https://github.com/jakemcc/test-refresh/blob/master/README.md#features) will always have the latest list but as of the time of writing this they include:

* Reloads code and reruns tests on changes to your project's code.
* Runs previously failing tests first.
* Supports [custom notification](https://github.com/jakemcc/test-refresh/blob/d6b2f2710ae94ae3270d92b0efd85d481717477b/sample.project.clj#L6-L9) commands.
* Built in [Growl](http://growl.info/) support.
* Can notify after test success and failure or [just after failure](https://github.com/jakemcc/test-refresh/blob/d6b2f2710ae94ae3270d92b0efd85d481717477b/sample.project.clj#L12).
* Supports a [subset](https://github.com/jakemcc/test-refresh/blob/master/CHANGES.md#040) of Leiningen test selectors.
* Reports on your tests running time.

I don't have enough experience with the new **lein-test-refresh** to say how having failing tests will affect my development practices.
I don't expect this to change my development practices but it will enable quicker feedback.
Quick feedback cycles are what it is all about.

### Acknowledgments ###

Most of the 'rerun failed tests first' feature was hashed out and spiked during a mob programming session organized by [Zee Spencer](http://www.zeespencer.com/).
This happened at a company conference put on by [Outpace](http://www.outpace.com/) in Las Vegas.
Many developers were involved but two that most influenced the final result were [Joel Holdbrooks](https://github.com/noprompt) and [Timothy Pratley](https://github.com/timothypratley).
