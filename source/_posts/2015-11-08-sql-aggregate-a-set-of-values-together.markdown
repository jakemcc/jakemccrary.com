---
layout: post
title: "SQL: Aggregate a set of values together"
date: 2015-11-08 13:45:46 -0600
comments: true
published: false
categories:
- sql
- postgres
- clojure
---

I've recently started working on projects that use
[Postgres](http://www.postgresql.org/) as our relational database. This
has allowed us to use features that Postgres supports to simplify
our code by moving some logic into the database queries. One SQL
function found in Postgres that has been used which greatly
simplified out code is the `array_agg`
[aggregate](http://www.postgresql.org/docs/9.4/static/functions-aggregate.html)
function.

## What is `array_agg`?

The `array_agg(expression)` function takes an expression and returns
an array of the argument type. It's probably easiest to explain with
an example. The snippet below shows a simplified schema for a database
describing a blog. There is a table called `blog_posts` that contains
details about individual blog posts, a table called `categories` that
has some labels that can be applied to individual blog posts, and a
join table called `post_categories` that links the two tables together.

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
            1 |           4
            2 |           2
            2 |           3
```

Now you want to know how each blog post has been categorized. Before I
learned about `array_agg` I would have done something similar to the
following query.

```
select title, name as category
  from blog_posts
  join post_categories pc on pc.blog_post_id = blog_posts.id
  join categories c on c.id = pc.category_id
  order by title;


    title     | category
--------------+----------
 Clojure Post | emacs
 Clojure Post | clojure
 SQL Post     | sql
 SQL Post     | postgres
```

With a small enough data set the above isn't difficult to read but it
becomes eventually becomes cumbersome. What you really want to read is
a single line per blog post. You can get this using `array_agg`.


```sql
select title, array_agg(name) as categories
  from blog_posts
  join post_categories pc on pc.blog_post_id = blog_posts.id
  join categories c on c.id = pc.category_id
  group by title;

    title     |   categories
--------------+-----------------
 SQL Post     | {sql,postgres}
 Clojure Post | {emacs,clojure}
```

I find the resulting data from the query using `array_agg` to be much
nicer to read. I think the query also better expresses the question
you're trying to answer.

## How did it make my Clojure code simpler?

I'm working with a schema that has many relationships similar to the
above relationship. Prior to using `array_agg` we needed to develop
and maintain code to translate from the one row per category result
to a single map per post. Example of data shapes below.

``` clojure
;; data shape you get from query.
[{:title "Clojure Post" :category "emacs"}
 {:title "Clojure Post" :category "clojure"}
 {:title "SQL Post" :category "sql"}
 {:title "SQL Post" :category "postgres"}]

;; data shape you want
[{:title "Clojure Post" :categories ["emacs" "clojure"]}
 {:title "SQL Post" :categories ["sql" "postgres"]}]
```

One way of reshaping the data is presented below. The code snippet has
quite a bit going on and is very specific to this translation.

``` clojure
(defn squash-by-title [rows]
  (->> rows
       (reduce (fn [r row] (update r (:title row) conj (:category row))) {})
       (map (fn [[title categories]] {:title title :categories categories}))))
```

In my experience you'll often start with something like this and then
start doing very similar squash operations on slightly different data.
Eventually you end up extracting a function (maybe called `squash`)
that performs the common operations. You'll feel great about this
extraction and then a few months later you'll finally write another
query that needs to use `squash`. In effort to understand it you'll
stare at it for a handful of minutes and then start looking for usages
in the rest of your code base. Luckily (if you're using a database
that supports `array_agg`) there is a better way.

If you start using `array_agg` in your queries than the only code that
needs to be maintained and understood is a pretty straight-forward
protocol extension.

``` clojure
;; clojure.java.jdbc has been required as jdbc

(extend-protocol jdbc/IResultSetReadColumn
  org.postgresql.jdbc4.Jdbc4Array
  (result-set-read-column [pgobj metadata i]
    (vec (.getArray pgobj))))
```

Those four lines of code let you use `array_agg` in your queries and
get back your data in the shape you want it.
