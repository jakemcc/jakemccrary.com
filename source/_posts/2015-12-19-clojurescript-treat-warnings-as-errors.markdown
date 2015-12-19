---
layout: post
title: ! 'ClojureScript: Treat warnings as errors'
date: 2015-12-19 17:09 -0600
comments: true
published: true
categories:
- clojurescript
- leiningen
- clojure
---

Recently my team deployed a new version of our ClojureScript UI and it
had a minor bug. It was trivial to fix the problem, a ClojureScript
build warning pointed us to the cause. As a result we started thinking
it would be nice to have build warnings count as errors and fail our
ClojureScript build.

We use [Leiningen](http://leiningen.org/) (version 2.5.3) and
[lein-cljsbuild](https://github.com/emezeske/lein-cljsbuild) (version
1.1.1). After some searching we found that lein-cljsbuild supports
[specifying custom warning handlers](https://github.com/emezeske/lein-cljsbuild#custom-warning-handlers)
as the value to the `:warning-handlers` key. The lein-cljsbuild README
even provides an example, which we took and added a `(System/exit 1)`
to the end of it.

```clojure
:warning-handlers [(fn [warning-type env extra]
                     (when-let [s (cljs.analyzer/error-message warning-type extra)]
                       (binding [*out* *err*]
                         (println "WARNING:" (cljs.analyzer/message env s)))
                       (System/exit 1)))]
```

This worked! Well, it sort of worked. Our build failed whenever there
was a warning but now we were seeing spurious warnings. We saw "Use of
undeclared Var" warnings when functions created in a `letfn` where
calling each other. Definitely not a situation that warrants a warning
and definitely not a build failure.

We weren't seeing this warning before so we opened ClojureScript's
source and found the
[default warning handler](https://github.com/clojure/clojurescript/blob/452edf43927566cc0ea0a3846706c0294cef235d/src/main/clojure/cljs/analyzer.cljc#L360-L366).
The default handler checks that the `warning-type` is set to true
`*cljs-warnings*`. Inspired by the default handler we added the same
check to the start of our warning handler.

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
build and deploy with a little more confidence.
