---
layout: post
title: "What are the most used Clojure libraries?"
date: 2017-04-12 19:19:32 -0500
comments: true
published: false
description: Let's use the power of Google's BigQuery to discover the most used Clojure libraries.
keywords: 'clojure, bigquery, libraries, library'
categories:
- clojure
- bigquery
- google
---

In
a
[previous post](/blog/2017/03/31/what-clojure-testing-library-is-most-used/),
we used Google's BigQuery and the
public
[GitHub dataset](https://cloud.google.com/bigquery/public-data/github)
to discover the most used Clojure testing library. The answer wasn't
surprising. The built-in `clojure.test` was by far the most used.

Let's use the dataset to answer a less obvious question, what are the
most used libraries in Clojure projects?

Before we can answer that question, we'll need to transform some
data. First, we create the Clojure subset of the GitHub dataset. I did
that by executing the following queries and saving them to a couple
tables.

```sql
-- Save the results of this query to the clojure.files table
SELECT
  *
FROM
  [bigquery-public-data:github_repos.files]
WHERE
  RIGHT(path, 4) = '.clj'
  OR RIGHT(path, 5) = '.cljc'
  OR RIGHT(path, 5) = '.cljs'
  OR RIGHT(path, 10) = 'boot.build'

-- Save the results to clojure.contents
SELECT *
FROM [bigquery-public-data:github_repos.contents]
WHERE id IN (SELECT id FROM clojure.files)
```

Next we extract the dependencies from `build.boot` and `project.clj`
files. Fortunately for us, both of these files specify dependencies in
the same format, so we're able to use the same regular expression on both types.

The query below identifies `project.clj` and `build.boot` files,
splits each file into lines, and extracts referenced library names and
versions using a regular expression. Additional filtering is done get
rid of some spurious results.

```sql
SELECT
  REGEXP_EXTRACT(line, r'\[+(\S+)\s+"\S+"]') AS library,
  REGEXP_EXTRACT(line, r'\[+\S+\s+"(\S+)"]') AS version, 
  COUNT(*) AS count
FROM (
  SELECT
    SPLIT(content, '\n') AS line
  FROM
    [clojure.contents]
  WHERE
    id IN (
    SELECT
      id
    FROM
      [clojure.files]
    WHERE
      path LIKE '%project.clj'
      OR path LIKE '%build.boot')
      HAVING line contains '[')
GROUP BY
  library, version
HAVING library is not null and not library contains '"'
ORDER BY
  count DESC
```

The first five rows results are below. Let's save the entire result to
a `clojure.libraries` table.

```
| library             | version | count |
|---------------------+---------+-------|
| org.clojure/clojure | 1.6.0   | 7015  |
| org.clojure/clojure | 1.5.1   | 4251  |
| org.clojure/clojure | 1.7.0   | 4093  |
| org.clojure/clojure | 1.8.0   | 3016  |
| hiccup              | 1.0.5   | 1280  |
```

Now we can start answering all sorts of interesting questions.

What is the most referenced library put out under the `org.clojure` group?

```
SELECT library, sum(count) count
FROM clojure.libraries
WHERE library CONTAINS 'org.clojure'
GROUP BY library
ORDER BY count desc

| Row | library                        | count |
|-----+--------------------------------+-------|
|   1 | org.clojure/clojure            | 20834 |
|   2 | org.clojure/clojurescript      |  3080 |
|   3 | org.clojure/core.async         |  2612 |
|   4 | org.clojure/tools.logging      |  1579 |
|   5 | org.clojure/data.json          |  1546 |
|   6 | org.clojure/tools.nrepl        |  1244 |
|   7 | org.clojure/java.jdbc          |  1064 |
|   8 | org.clojure/tools.cli          |  1053 |
|   9 | org.clojure/tools.namespace    |   982 |
|  10 | org.clojure/test.check         |   603 |
|  11 | org.clojure/core.match         |   578 |
|  12 | org.clojure/math.numeric-tower |   503 |
|  13 | org.clojure/data.csv           |   381 |
|  14 | org.clojure/math.combinatorics |   372 |
|  15 | org.clojure/tools.reader       |   368 |
|  16 | org.clojure/clojure-contrib    |   335 |
|  17 | org.clojure/data.xml           |   289 |
|  18 | org.clojure/tools.trace        |   236 |
|  19 | org.clojure/java.classpath     |   199 |
|  20 | org.clojure/core.cache         |   179 |
```

Clojure and ClojureScript are at the top, which isn't surprising. I'm
surprised to see `tools.nrepl` in the next five results (rows 3-7). It
is the only library out of the top that I haven't used.

What testing library is used the most? We already answered this in
my
[last article](/blog/2017/03/31/what-clojure-testing-library-is-most-used/) but
let's see if we get the same answer when we're counting how many times
a library is pulled into a project.

```
SELECT library, sum(count) count
FROM [clojure.libraries] 
WHERE library in ('midje', 'expectations', 'speclj', 'smidjen', 'fudje')
GROUP BY library
ORDER BY count desc

| Row | library                | count |
|-----+------------------------+-------|
|   1 | midje                  |  1122 |
|   2 | speclj                 |   336 |
|   3 | expectations           |   235 |
|   4 | smidjen                |     1 |
```

Those results are close to the previous results. Of the non-clojure.test libraries, midje still ends up on top.

What groups (as identified by the Maven groupId) have their libraries referenced the most? Top 12 are below but the [full result](https://docs.google.com/a/jakemccrary.com/spreadsheets/d/1QGRRGSo5t5Pnpwizdv_H8negs8NBxtRour6KxWN6hVY/edit?usp=sharing) is available.

```
SELECT REGEXP_EXTRACT(library, r'(\S+)/\S+') AS group, sum(count) AS count
FROM [clojure.libraries]
GROUP BY group
HAVING group IS NOT null
ORDER BY count DESC

| Row | group                 | count |
|-----+-----------------------+-------|
|   1 | org.clojure           | 39611 |
|   2 | ring                  |  5817 |
|   3 | com.cemerick          |  2053 |
|   4 | com.taoensso          |  1605 |
|   5 | prismatic             |  1398 |
|   6 | org.slf4j             |  1209 |
|   7 | cljsjs                |   868 |
|   8 | javax.servlet         |   786 |
|   9 | com.stuartsierra      |   642 |
|  10 | com.badlogicgames.gdx |   586 |
|  11 | cider                 |   560 |
|  12 | pjstadig              |   536 |
```

And finally, the question that inspired this article, what is the most used library?

```
SELECT library, sum(count) count
FROM [clojure.libraries]
WHERE library != 'org.clojure/clojure'
GROUP BY library
ORDER BY count desc

| Row | library                     | count |
|-----+-----------------------------+-------|
|   1 | compojure                   |  3609 |
|   2 | lein-cljsbuild              |  3413 |
|   3 | org.clojure/clojurescript   |  3080 |
|   4 | org.clojure/core.async      |  2612 |
|   5 | lein-ring                   |  1809 |
|   6 | cheshire                    |  1802 |
|   7 | environ                     |  1763 |
|   8 | ring                        |  1678 |
|   9 | clj-http                    |  1648 |
|  10 | clj-time                    |  1613 |
|  11 | hiccup                      |  1591 |
|  12 | lein-figwheel               |  1582 |
|  13 | org.clojure/tools.logging   |  1579 |
|  14 | org.clojure/data.json       |  1546 |
|  15 | http-kit                    |  1423 |
|  16 | lein-environ                |  1325 |
|  17 | ring/ring-defaults          |  1302 |
|  18 | org.clojure/tools.nrepl     |  1244 |
|  19 | midje                       |  1122 |
|  20 | com.cemerick/piggieback     |  1096 |
|  21 | org.clojure/java.jdbc       |  1064 |
|  22 | org.clojure/tools.cli       |  1053 |
|  23 | enlive                      |  1001 |
|  24 | ring/ring-core              |   995 |
|  25 | org.clojure/tools.namespace |   982 |
```

[Compojure](https://github.com/weavejester/compojure) takes the top
slot. [Full results are available](https://docs.google.com/a/jakemccrary.com/spreadsheets/d/1-zmcOVPKLGrdRT_VkTrRUuRFyuxxmXi9eeH6Xzlt7yg/edit?usp=sharing).

Before doing this research I tried to predict what libraries I'd see
in the top 10. I predicted that clj-time and clj-http would be up
there. They barely made it in.

It was pretty pleasant using BigQuery to do this analysis. Queries
executed quickly with each query finishing within seconds of starting.
