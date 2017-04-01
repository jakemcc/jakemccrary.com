---
layout: post
title: Which Clojure testing library is most used?
date: 2017-03-31 21:54 -0500
comments: true
published: true
description: Let's use Google's BigQuery to discover the most widely used Clojure
  testing library.
keywords: clojure, clojure.test, expectations, midje, fudje, speclj, bigquery
categories:
- clojure
- testing
- google
- bigquery
---

I've always assumed that the built-in `clojure.test` is the most
widely used testing library in the Clojure community. Earlier this
month I decided to test this assumption using the
Google's BigQuery [GitHub dataset](https://cloud.google.com/bigquery/public-data/github).

The BigQuery GitHub dataset contains over three terabytes of source
code from more than 2.8 million open source GitHub
repositories. BigQuery lets us quickly query this data using SQL.

Below is a table with the results (done in early March 2017) of my
investigation. Surprising no one, `clojure.test` comes out as the
winner and it is a winner by a lot.
 
```
| Library      | # Repos Using |
|--------------+---------------|
| clojure.test |         14304 |
| midje        |          1348 |
| expectations |           429 |
| speclj       |           207 |
| smidjen      |             1 |
| fudje        |             1 |
```

23,243 repositories were identified as containing Clojure (or
ClojureScript) code. This means there were about 6,953 repositories
that didn't use any testing library[^1]. This puts the "no tests or an
obscure other way of testing" in a pretty solid second place.

You should take these numbers as ballpark figures and not exact
answers. I know from using GitHub's search interface that there are
three public projects
using [fudje](https://github.com/jimpil/fudje)[^2].

So, why don't all three of those projects show up? The dataset only
includes projects where Google could identify the project as open
source and the GitHub licenses API is used to do that[^3]. Two of
those three projects were probably unable to be identified as
something with an appropriate license.

Another small problem is that since `expectations` is an actual word,
it shows up outside of `ns` declarations. I ended up using a fairly
simple query to generate this data and it only knows that
`expectations` shows up somewhere in a file. I experimented with some
more restrictive queries but they didn't drastically change the result
and I wasn't sure they weren't wrong in other ways. If you subtract a
number between 100 and 150 you'll probably have a more accurate
expectations usage count.

Keep reading if you want to hear more about the steps to come up with
the above numbers.

If you have other Clojure questions you think could be answered by
querying this dataset, let me know in the comments or
on [twitter](https://twitter.com/jakemcc). I have some more ideas, so
I wouldn't be surprised if at least one more article gets written.



[^1]: Probably higher, as projects can and use more than one testing library.

[^2]: And those projects are [jumarko/clojure-random](https://github.com/jumarko/clojure-random), [dpassen1/great-sort](https://github.com/dpassen1/great-sort), and [jimpil/fudje](https://github.com/jimpil/fudje).

[^3]: [Source is a Google Developer Advocate's response on old HN post](https://news.ycombinator.com/item?id=12004644)

## The Details

The process was pretty straightforward. Most of my time was spent
exploring the tables, figuring out what the columns represented,
figuring out what queries worked well, and manually confirming some of
the results. BigQuery is very fast. Very little of my time was spent
waiting for results.

### 1. Setup the data

You get 1 TB of free BigQuery usage a month. You can blow through this
in a single query. Google provides sample tables that contain less
data but I wanted to operate on the full set of Clojure(Script) files,
so my first step was to execute some queries to create tables
that only contained Clojure data.

First, I queried the `github_repos.files` table for all the
Clojure(Script) files and saved that to a `clojure.files` table.

```sql
SELECT
  *
FROM
  [bigquery-public-data:github_repos.files]
WHERE
  (RIGHT(path, 4) = '.clj'
    OR RIGHT(path, 5) = '.cljc'
    OR RIGHT(path, 5) = '.cljs')
```

The above query took only 9.2 seconds to run and processed 328 GB of data.

Using the `clojure.files` table, we can select the source for all the
Clojure code from the `github_repos.contents`. I saved this to a
`clojure.contents` table.

```sql
SELECT *
FROM [bigquery-public-data:github_repos.contents]
WHERE id IN (SELECT id FROM clojure.files)
```

This query processed 1.84 TB of data in 21.5 seconds. So fast. In just
under 30 seconds, I've blown through the free limit.

### 2. Identify what testing library (or libraries) a repo uses

We can guess that a file uses a testing library if it contains certain
string. The strings we'll search for are the namespaces we'd expect to
see required or used in a `ns` declaration. The below query does this
for each file and then rolls up the results by repository. It took 3
seconds to run and processed 611 MB of data.

```
SELECT
  files.repo_name,
  MAX(uses_clojure_test) uses_clojure_test,
  MAX(uses_expectations) uses_expectations,
  MAX(uses_midje) uses_midje,
  MAX(uses_speclj) uses_speclj,
  MAX(uses_fudje) uses_fudje,
  MAX(uses_smidjen) uses_smidjen,
FROM (
  SELECT
    id,
    contents.content LIKE '%clojure.test%' uses_clojure_test,
    contents.content LIKE '%expectations%' uses_expectations,
    contents.content LIKE '%midje%' uses_midje,
    contents.content LIKE '%speclj%' uses_speclj,
    contents.content LIKE '%fudje%' uses_fudje,
    contents.content LIKE '%smidjen%' uses_smidjen,
  FROM
    clojure.contents AS contents) x
JOIN
  clojure.files files ON files.id = x.id
GROUP BY
  files.repo_name
```

Below is a screenshot of the first few rows in the result.

![BigQuery results for test library usage by repo](/images/bigquery-testing-library-result.png "BigQuery results for test library usage by repo")

### 3. Export the data

At this point, we could continue doing the analysis using SQL and the
BigQuery UI but I opted to explore the data using Clojure and the
repl. There were too many rows to directly download the query results
as a csv file, so I ended up having to save the results as a table and
then export it to Google's cloud storage and download from there.

The first few rows of the file look like this:

```
files_repo_name,uses_clojure_test,uses_expectations,uses_midje,uses_speclj,uses_fudje,uses_smidjen
wangchunyang/clojure-liberator-examples,true,false,false,false,false,false
yantonov/rex,false,false,false,false,false,false
```

### 4. Calculate some numbers

The code takes the csv file and does some transformations. You could
do this in Excel or using any language of your choice. I'm not going
to include code here, as it isn't that interesting.

## BigQuery thoughts

This was my first time using Google's BigQuery. This wasn't the most
difficult analysis to do but I was impressed at the speed and ease of
use. The web UI, which I used entirely for this, is neither really
great or extremely terrible. It mostly just worked and I rarely had to
look up documentation.

I don't really feel comfortable making a judgment call on if the cost
is expensive or not but this article cost a bit less than seven
dollars to write. This doesn't seem to outrageous to me.

Based on my limited usage of BigQuery, it is something I'd look into further if I needed its capabilities.

