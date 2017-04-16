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
my
[previous post](/blog/2017/03/31/what-clojure-testing-library-is-most-used/) we
used Google's BigQuery and the
public
[GitHub dataset](https://cloud.google.com/bigquery/public-data/github)
to discover the most used Clojure testing library. The answer wasn't
surprising. The built-in `clojure.test` was by far the most used.

Let's use the dataset to answer a less obvious question, what
libraries are used the most in Clojure projects?

## The Setup

In early March I ran a couple queries to create a Clojure specific
subset of the public GitHub
dataset. BigQuery [charges](https://cloud.google.com/bigquery/pricing)
you based on how much data your queries process and on storage
used. Querying the whole GitHub dataset blows through the free limits
of BigQuery. It is recommended that you select a subset of data you
care about and then do your future queries against that data. Below
are the queries I used to create a Clojure subset.

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
  OR path CONTAINS 'boot.build'

-- Save the results to clojure.contents
SELECT *
FROM [bigquery-public-data:github_repos.contents]
WHERE id IN (SELECT id FROM clojure.files)
```

Next we extract the dependencies from `build.boot` and `project.clj`
files. Fortunately for us, both of these files specify dependencies in
the same format, so we're able to use the same regular expression on both types.

The query below identifies `project.clj` and `build.boot` files,
splits the file content into lines, and uses a regular expression to
extract the library name and version. There is some filtering done to
get rid of some spurious results.

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

The first five rows in the results are below. Let's save the entire
result to a `clojure.libraries` table.

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

Clojure and ClojureScript are at the top, which isn't suprising. Out of the next five results (rows 3 - 7), `tools.nrepl` surprises me. It is the only library out of the top that I haven't used.

In the [last post](/blog/2017/03/31/what-clojure-testing-library-is-most-used/) we looked into what testing libraries were used the most by performing queries against file contents. Let's answer that same question by looking at libraries extracted this way.

```
SELECT library, sum(count) count
FROM [clojure.libraries] 
WHERE library in ('midje', 'expectations', 'speclj', 'smidjen', 'fudje', 'org.clojure/test.check')
GROUP BY library
ORDER BY count desc

| Row | library                | count |
|-----+------------------------+-------|
|   1 | midje                  |  1122 |
|   2 | org.clojure/test.check |   603 |
|   3 | speclj                 |   336 |
|   4 | expectations           |   235 |
|   5 | smidjen                |     1 |
```

Those results are fairly close to the previous results. Of the non-clojure.test libraries, midje still ends up on top.


