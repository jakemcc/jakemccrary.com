---
layout: post
title: "Using comm to verify matching content"
date: 2017-05-29 09:12:32 -0500
comments: true
published: false
description: comm is a useful command line tool for looking for common and unique lines in files.
keywords: 'comm, utilities, linux, sort, cut'
categories: 
- utilities
- linux
---

I recently found myself in a situation where I needed to confirm that a process took in a tab separated file, did some processing, and then output a new file containing the original columns with some additional ones. The feature I was adding allowed the process to die and restart while processing the input file and pick up where it left off.

I needed to confirm the output had data for every line. I hadn't used it in a while, but I reached to `comm` to help me out.

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

With files this size, it would be easy enought to just visually check. In my testing, I was dealing with files that had thousands of lines. This is way too many to check by hand. I don't think I had used `comm` in years, but it worked great.

[comm](https://en.wikipedia.org/wiki/Comm) reads to files as input and outputs three columns. The first column contains lines found only in the first file, the second column contains lines only found in the second, and the last file contains lines in both. Below is an example adapted from Wikipedia showing its behavior.

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

So how was `comm` useful to me? Well, you can also ask it to suppress specfic columns from its output. This allows me to send the first two columns of each file to `comm` and suppress `comm`'s third column. Anything that gets printed to the screen is unique to one of the inputs and I can consider that a failure since the output file should have a row for every row in the input file. Let's see what happens. We'll use `cut` to select the first two columns and then sort the output. `comm` expects its input to be sorted.

```
$ comm -3 <( cut -f 1,2 input.txt | sort ) <( cut -f 1,2 output.txt | sort )
$
```

Success! There is nothing unique in either file.
