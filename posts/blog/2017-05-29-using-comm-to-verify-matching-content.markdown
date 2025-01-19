---
dated-url: true
layout: post
title: Using comm to verify file content matches
date: 2017-05-29 10:45 -0500
comments: true
published: true
description: comm is a useful command line tool for looking for common and unique
  lines in files.
keywords: comm, utilities, linux, sort, cut, osx, tools
categories:
- utilities
- linux
- osx
- tools
---

I recently found myself in a situation where I needed to confirm that a process took in a tab separated file, did some processing, and then output a new file containing the original columns with some additional ones. The feature I was adding allowed the process to die and restart while processing the input file and pick up where it left off.

I needed to confirm the output had data for every line in the input. I reached to the command line tool `comm`.

Below is a made up input file.

```
UNIQUE_ID	USER
1	38101838
2	19183819
3	19123811
4	10348018
5	19881911
6	29182918
```

And here is some made up output.

```
UNIQUE_ID	USER	MESSAGE
1	38101838	A01
2	19183819	A05
3	19123811	A02
4	10348018	A01
5	19881911	A02
6	29182918	A05
```

With files this size, it would be easy enough to check visually. In my testing, I was dealing with files that had thousands of lines. This is too many to check by hand. It is a perfect amount for `comm`.

[comm](https://en.wikipedia.org/wiki/Comm) reads two files as input and then outputs three columns. The first column contains lines found only in the first file, the second column contains lines only found in the second, and the last column contains lines in both. If it is easier for you to think about it as set operations, the first two columns are similar to performing two set differences and the third is similar to set intersection. Below is an example adapted from Wikipedia showing its behavior. 


```
$ cat foo.txt
apple
banana
eggplant
$ cat bar.txt
apple
banana
banana
zucchini
$ comm foo.txt bar.txt
                  apple
                  banana
          banana
eggplant
          zucchini
```

So how is this useful? Well, you can also tell `comm` to suppress outputting specific columns.  If we send the common columns from the input and output file to `comm` and suppress `comm`'s third column then anything printed to the screen is a problem. Anything printed to the screen was found in one of the files and not the other. We'll select the common columns using cut and, since comm expects input to be sorted, then sort using `sort`. Let's see what happens.

```
$ comm -3 <(cut -f 1,2 input.txt | sort) <(cut -f 1,2 output.txt | sort)
$
```

Success! Nothing was printed to the console, so there is nothing unique in either file.

`comm` is a useful tool to have in your command line toolbox.
