---
layout: post
title: Command line arguments in Clojure
date: 2011-04-12 20:00
updated: 2011-08-06
categories: [clojure, code]
---

Write enough Clojure and eventually you will need to handle command line arguments. There are numerous ways of doing this. Keep reading for a brief introduction to three.

### Using built-in features

There exists a sequence named `*command-line-args*` which contains the arguments to your application. Using it is simple, it is just a sequence after all, and it is always available to you. No need to pull in external dependencies that others may not be familiar with.

This simplicity is also a downside. Because only a sequence is provided for you it is up to you to actually figure out the arguments. If you want to do any sort of verification that certain arguments are supplied you write the code that does the verifying. If you want to move away from positional arguments to using command line flags once again it is up to you to write it.

Because of the amount of code required to do any sort of advanced argument handling I tend to use `*command-line-args*` only for applications that take a single type of argument, for example a file path, and require one or more of this type of argument.

### Setup for next two sections

For the next two sections I'm using version 1.5.0 of Leiningen and the specified versions of libraries as stated in the below `project.clj` file.

``` clojure
    (defproject blogpost "1.0.0-SNAPSHOT"
      :dependencies [[org.clojure/clojure "1.2.0"]
                     [org.clojure/clojure-contrib "1.2.0"]
                     [clargon "1.0.0"]]
      :dev-dependencies [[swank-clojure "1.2.1"]]
      :run-aliases {:clargon clargon-example
                    :cc command-line-example})
```

I'm using `lein run` to run the examples. `lein run :cc` runs the clojure.contrib example. Likewise, running `lein run :clargon` will run the clargon examples. Both of these commands can be followed by additional arguments that get passed to the application.

### Using clojure.contrib.command-line

The next step after using `*command-line-args*` is to use the library `clojure.contrib.command-line`. This library provides the function `with-command-line` that allows you specify requirements and then handles the parsing of the command line arguments for you.

Positives of using `clojure.contrib.command-line`:
* Part of `clojure.contrib`. Probably extremely low friction to start using it.
* No longer need to write your own command line parsing code.
* Responds to `-h` and `--help`.

A negative of using `clojure.contrib.command-line` is that the documentation is pretty sparse. This can lead to some fumbling around as you learn how to use it. Another downside is that there isn't a way of specifying whether an argument is required or optional. This means you must manually check for required arguments and give appropriate error messages to the user.

Below is an example of using `clojure.contrib.command-line`. It specifies a few different arguments. The `--cow` argument has a default value of "cow". `--chicken` has no default value, if it is left unspecified it will be `nil`. The line with `milk?` specifies a boolean value. If `--milk` (or `-m` because of the `m?` specification) is specified at the command line then `milk?` will be true. `extras` will collect any additional arguments.

``` clojure
    (ns command-line-example
      (:require [clojure.contrib.command-line :as ccl]))

    (defn -main [& args]
      (ccl/with-command-line args
        "Command line demo"
        [[cow "This is the cows name" "cow"]
         [chicken "This specifies the chickens name"]
         [milk? m? "Should you milk the cow?"]
         extras]
        (println "cow's name: " cow)
        (println "chicken's name: " chicken)
        (println "milk?: " milk?)
        (println "extra args: " extras)))
```

And here is an example of calling that `-main` function from the repl.

``` bash
    $ lein run :cc --cow Herb --milk other args
    cow's name:  Herb
    chicken's name:  nil
    milk?:  true
    extra args:  [other args]
```

### Using some other library

Another option is to use some library that isn't found in `clojure.contrib`. One example of this is [clargon](https://github.com/gar3thjon3s/clargon). Clargon is a library that [Gaz Jones](http://blog.gaz-jones.com/) (his blog post [here](http://blog.gaz-jones.com/post/2528825514/command-line-applications-in-clojure)) wrote. The documentation (both in his blog post and through the github page and tests) is the primary reason I started using it.

Pros of clargon:
* Great documentation. Makes it quick to get started.
* Can specify functions to transform arguments prior to gaining access to them
* You specify if an argument is required or optional.
* Responds to `-h` and `--help`.

One potential negative of using clargon is that it isn't a `clojure.contrib` library. This means there is slightly more friction to start using it on your project as, unlike `clojure.contrib`, you are probably not already depending on it.

Below is an example similar to the above `clojure.contrib.command-line` example. One important difference is that some arguments are now specified as either required or optional. If a required argument is not specified then an error is printed and execution stops.

``` clojure
    (ns clargon-example
      (:require [clargon.core :as c]))

    (defn -main
      [& args]
      (let [opts
            (c/clargon
             args
             (c/optional ["--cow" "Specify the cow's name" :default "cow"])
             (c/required ["--chicken" "Chicken's name"])
             (c/optional ["-m?" "--milk?" "should you milk the cow?"]))]
        (println args)
        (println opts)))
```

`optional` and `required` both take a vector that defines the specification of a flag. Starting with the first element in that vector, each element that is a string and starts with a '-' is considered a potential flag for that argument. The last flag is stripped of leading '-' characters and is considered the name of that flag (unless a `:name` option is specified later). The name is used to look up the value of the argument in the option map that is returned by the `clargon` function. If the next element after the last flag is a string then it is considered the documentation for that flag. When clargon runs into a non-string element then it and everything after it are considered options and should be specified as key value pairs. Options that do something are `:default`, `:name`, and `:required`.

`optional` and `required` both can take a function as a second argument. This function will be passed the argument for that flag and should return a transformed version of it. Below is an example using this functionality to specify a required flag that takes a comma separated list of files. These comma separated files are split apart and stuck into a vector.

``` clojure
    (ns clargon-example
      (:require [clargon.core :as c]))

    (defn -main
      [& args]
      (let [opts (c/clargon
                  args
                  (c/required ["--files" "Files to process"]
                              #(vec (.split % ","))))]
        (println "Parsed opts: " opts)))
```

Below is the above example being ran.

``` clojure
    $ lein run :clargon --files one.txt,two.txt,three.txt
    Parsed opts:  {:files [one.txt two.txt three.txt]}
```

Clargon supports some more advanced nested argument handling that I'm not going to go into here. If you want to know more about clargon I'd recommend reading reading Gaz's [blog post](http://blog.gaz-jones.com/post/2528825514/command-line-applications-in-clojure) and the [clargon](https://github.com/gar3thjon3s/clargon) readme and tests.

### End

There are many more ways to handle command line parsing in Clojure. You are not limited to any of the three above. I've personally found clargon to hit all of my needs and plan on continuing to use it.


