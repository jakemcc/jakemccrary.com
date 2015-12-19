---
layout: post
title: "ClojureScript: Treating warnings as errors"
date: 2015-12-19 14:54:43 -0600
comments: true
published: false
categories: 
- clojurescript
- leiningen
- clojure
---

Recently my team pushed a new version of our ClojureScript UI and it
had a minor bug in it. It turns out that our ClojureScript build was
emitting a warning, which completely identified the problem, but
because warnings don't fail the build we were able to overlook it.

Our build uses [Leiningen](http://leiningen.org/) (version 2.5.3) and
[lein-cljsbuild](https://github.com/emezeske/lein-cljsbuild) (version
1.1.1) and after searching around found that lein-cljsbuild supports
[specifying custom warning handlers](https://github.com/emezeske/lein-cljsbuild#custom-warning-handlers)
as the value to the `:warning-handlers` key. Great, this provides a
point where we can change the behavior with regards to warnings.

Our first attempt took the example from lein-cljsbuild's readme and
added a `(System/exit 1)` at the end of it.

```clojure
:warning-handlers [(fn [warning-type env extra]
                     (when-let [s (cljs.analyzer/error-message warning-type extra)]
                       (binding [*out* *err*]
                         (println "WARNING:" (cljs.analyzer/message env s)))
                       (System/exit 1)))]
```

This (kind of) worked! Our build started to fail whenever there was a
warning! The problem we now faced was that we were seeing warnings
that shouldn't have been warnings. We saw "Use of undeclared Var"
warnings when functions created in a `letfn` where calling each other.
Definitely not a situation that warrants a warning and definitely not
a build failure. 

We weren't seeing this false warning before so we took a peek at
ClojureScript's source and found the
[default warning handler](https://github.com/clojure/clojurescript/blob/452edf43927566cc0ea0a3846706c0294cef235d/src/main/clojure/cljs/analyzer.cljc#L360-L366).
The default handler checks that the `warning-type` is set to true
`*cljs-warnings*`. Below is our warning handler with that additional check.

```clojure
:warning-handlers [(fn [warning-type env extra]
                     (when (warning-type cljs.analyzer/*cljs-warnings*)
                       (when-let [s (cljs.analyzer/error-message warning-type extra)]
                         (binding [*out* *err*]
                           (println "WARNING:" (cljs.analyzer/message env s)))
                         (System/exit 1))))]
```

Success! Now we no longer get incorrect warnings when compiling our
`letfn` form and our build still fails if a warning occurs. Now we can
deploy with a little bit more confidence.
