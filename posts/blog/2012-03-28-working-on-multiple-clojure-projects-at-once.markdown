---
dated-url: true
layout: post
title: "Working on multiple Clojure projects at once"
date: 2012-03-28 21:18
comments: true
categories: [clojure]
---

Very few coders would debate the wisdom of breaking a project into smaller libraries. One complaint about breaking a project into tinier libraries is the added hassle of making changes simultaneously to multiple projects at once. Constantly releasing a library so another project can pick up changes is annoying and slows you down. Luckily for us in a [Clojure](http://clojure.org) project using [Leiningen](https://github.com/technomancy/leiningen) it is simple to make changes to a library and then use those changes without needing to perform a release.

This is accomplished by using the `checkouts` directory feature of Leiningen. This is a feature that, despite being listed in the Leiningen FAQ, I only recently discovered. To make your Clojure project (from now on calling this the main project) depend on the source of another project simply make a `checkouts` directory in your main project's root directory and then in `checkouts` link to the root of the library's project. This causes the library to be added to the main project's classpath. Now you can make changes to the main project and its dependencies without going through the hassle of releasing new versions of the library for every change.

In case the above paragraph isn't clear, here is an example of the main projects directory structure.
``` bash
    $ pwd
    src/main-project
    $ tree
    .
    ├── checkouts
    │   └── subproject -> /Users/jmccrary/src/temp/subproject/
    ├── project.clj
    ├── src
    │   └── main_project
    │       └── core.clj
    └── test
        └── main_project
            └── core_test.clj
    $ ls checkouts/subproject/
    README project.clj src test
```

Running a `lein classpath` in the main project directory and we can see the classpath has the subproject in it. I've edited the `lein classpath` output to remove most entries not related to subproject and to make it easier to read. As the example shows the subproject has been added to the classpath.

``` bash
    $ lein classpath
    ...:
    src/main-project/checkouts/subproject/src
    src/main-project/checkouts/subproject/classes
    src/main-project/checkouts/subproject/resources
    src/main-project/lib/clojure-1.3.0.jar
```

The Leiningen checkouts directory option is pretty useful. This feature isn't there to discourage you from releasing versions of a library, but instead is there to facilitate quicker development cycles. I'd encourage you to experiment with it and figure out of it makes you more effective.
