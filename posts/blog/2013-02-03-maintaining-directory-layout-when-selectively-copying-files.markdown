---
dated-url: true
layout: post
title: "Maintaining Directory Layout When Selectively Copying Files"
date: 2013-02-03 12:24
comments: true
categories: [bash, Linux, command-line]
---

Ran into a situation where I needed to replace certain files in a directory tree with files from a similarly shaped directory tree. The other files in each tree needed to remain untouched. Below is an example of the directory structure.

``` bash
    root-dir
    ├── target-dir
    │   ├── 20121230
    │   │   ├── data.csv
    │   │   └── instruments.csv
    │   └── 20121231
    │       ├── data.csv
    │       └── instruments.csv
    └── other-dir
        ├── 20121230
        │   ├── data.csv
        │   └── instruments.csv
        └── 20121231
            ├── data.csv
            └── instruments.csv
```

Goal is to copy **instruments.csv** from the sub-directories of **other-dir** to the matching sub-directories of **target-dir**. In the past I've solved this by being in the **other-dir** directory and banging out a `for` loop at the command line (`other-dir$` is the bash prompt). 

``` bash
    other-dir$ for d in $(ls); do cp $d/instruments.txt ../target-dir/$d/; done
```

One feature (or issue) with this approach is that if a sub-directory exists in **other-dir** but not in **target-dir** that sub-directory will not be created in **target-dir**.

I took a bit of time to explore other ways of accomplishing this task and stopped after coming up with two additional ways.

``` bash
    other-dir$ find . -name "instruments.txt" | xargs -I {} cp {} ../target-dir/{} 
```

The above is basically the same as the first solution. It uses `find` to generate the list of files and then constructs `cp` commands. It also doesn't create sub-directories in **target-dir**.

The next example has different behavior from the above `cp` solutions. Like the second solution, it generates a list of files to copy using `find` but then uses `rsync` with the `--files-from` flag to mirror those files under **target-dir**. Unlike the `cp` based solutions, sub-directories of **other-dir** that do not exist in **target-dir** will be created.

```
    other-dir$ find . -name "instruments.txt" | rsync --files-from=- . ../target-dir
```

I'm sure there are many more ways of accomplishing this task. Figuring out the three above was enough for me. They are fairly straight forward and don't depend on esoteric command line flags. The solution I use in the future will depend on whether or not I need sub-directories created in the target directory.


