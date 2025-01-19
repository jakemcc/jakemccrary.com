---
dated-url: true
layout: post
title: ! 'SQL: Aggregate a set of values together'
date: 2015-11-15 19:45 -0600
comments: true
published: true
description: The array_agg function is awesome and can simplify your code.
categories:
- sql
- postgres
- clojure
---

Lately I've been working on projects that use
[Postgres](http://www.postgresql.org/) as our relational database.
This has allowed us to simplify some of our Clojure code by leaning on
some built-in features of Postgres. One SQL function supported by
Postgres which has greatly simplified our code is the `array_agg`
[aggregate](http://www.postgresql.org/docs/9.4/static/functions-aggregate.html)
function.

## What is `array_agg`?

The `array_agg` function takes an argument and returns an array of the
argument type. That sentence will make more sense after an example.
The snippet below shows a simplified schema for a blog's database.
There is a table called `blog_posts` that contains details about
posts, a table called `categories` that has labels that can be applied
to blog posts, and a join table called `post_categories` that links
the two previous tables together.

```sql
blog=# select id, title from blog_posts;
 id |    title
----+--------------
  1 | SQL Post
  2 | Clojure Post

blog=# select * from categories;
 id |   name
----+----------
  1 | sql
  2 | emacs
  3 | clojure
  4 | postgres

blog=# select * from post_categories;
 blog_post_id | category_id
--------------+-------------
            1 |           1
            2 |           2
            1 |           4
            2 |           3
```

Before I learned about `array_agg`, if I wanted to know how each blog
post had been categorized I might have written the following query.

```
select title, name as category
  from blog_posts bp
  join post_categories pc on pc.blog_post_id = bp.id
  join categories c on c.id = pc.category_id
  order by title;


    title     | category
--------------+----------
 Clojure Post | emacs
 Clojure Post | clojure
 SQL Post     | sql
 SQL Post     | postgres
```

The result is readable but as the number of posts and categories grow
it becomes harder to read. The query also doesn't answer the question,
"How are my posts categorized?", well. The ideal answer is a single
row per post that shows the post's categories. You can use `array_agg`
to get that ideal answer.

```sql
select title, array_agg(name) as categories
  from blog_posts bp
  join post_categories pc on pc.blog_post_id = bp.id
  join categories c on c.id = pc.category_id
  group by title;

    title     |   categories
--------------+-----------------
 SQL Post     | {sql,postgres}
 Clojure Post | {emacs,clojure}
```

I find the `array_agg` version much nicer to read. The result answers
the question in a very direct fashion and the query expresses the
question well. Everything about the query expresses the question, you
no longer have an extra `order by` clause to make the result more
readable by human eyes.

## How did it make my Clojure code simpler?

The above is great and it makes everything more readable for a human.
Most of the time I'm not querying a SQL database so that a human can
directly read the results; instead I'm using Clojure to manipulate
results of a query. Fortunately, `array_agg` simplifies my Clojure
code as well.

I'm working with a schema that has many relationships similar to the
above relationship. Continuing with the example from above the snippet
below shows the data shape we'd get back from `clojure.java.jdbc`
prior to using `array_agg`. The data shape we actually want follows.

``` clojure
;; data shape you get from the non-array_agg query.
[{:title "Clojure Post" :category "emacs"}
 {:title "SQL Post" :category "sql"}
 {:title "Clojure Post" :category "clojure"}
 {:title "SQL Post" :category "postgres"}]

;; data shape you want
[{:title "Clojure Post" :categories ["emacs" "clojure"]}
 {:title "SQL Post" :categories ["sql" "postgres"]}]
```

Since we're not getting data in our desired shape we need to write
code that combines rows. One way of doing that is to use `reduce` and `map`.

``` clojure
(defn squash-by-title [rows]
  (->> rows
       (reduce (fn [r row] (update r (:title row) conj (:category row))) {})
       (map (fn [[title categories]] {:title title :categories categories}))))
```

I've been writing Clojure for a long time and when I see code like
above it still takes me a bit of time to figure out what is happening.
Not only that, but eventually your project has different squash
operations depending on what data you're pulling back from the
database. They are probably mostly similar and eventually you abstract
the differences and feel great. Then you come back months later and
have to figure out how it all works. Luckily, if you're using a
database that supports `array_agg`, there is a better way.

The first step is to change your queries to use `array_agg`. The
second step is to extend the `clojure.java.jdbc/IResultSetReadColumn`
protocol to the type returned by your jdbc driver. For my project that
looks like the following code:

``` clojure
;; clojure.java.jdbc has been required as jdbc

(extend-protocol jdbc/IResultSetReadColumn
  org.postgresql.jdbc4.Jdbc4Array
  (result-set-read-column [pgobj metadata i]
    (vec (.getArray pgobj))))
```

By changing my queries to use `array_agg` and adding those four lines
of code I'm able to delete all of my squashing functions and get data
from my database in the shape I want. I also end up with easier to
understand code and more expressive queries. Awesome.

_Thanks to [Timothy Pratley](http://timothypratley.blogspot.com/) for
providing feedback on earlier versions of this post._
