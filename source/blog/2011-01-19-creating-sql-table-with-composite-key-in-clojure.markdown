---
dated-url: true
comments: true
layout: post
title: Creating a SQL table with a composite primary key in Clojure
categories: [clojure, code, sql]
date: "2011-01-19"
---

I was interacting with a SQL database using Clojure and needed to create a table so I turned to `create-table` from [clojure.contrib.sql](http://clojure.github.com/clojure-contrib/sql-api.html).
Looking at the [docs](http://clojure.github.com/clojure-contrib/sql-api.html#clojure.contrib.sql/create-table) for `create-table` it seemed pretty straight forward.
To create a table with columns _date_, _id_, _symbol_, _price_, and _quantity_ you would write the following.

``` clojure
(create-table "orders"
              [:date     "date"]
              [:id       "integer"]
              [:symbol   "char(10)"]
              [:price    "integer"]
              [:quantity "integer"])
```

The above works.
I also wanted to specify that columns _date_ and _id_ to form a composite primary key.
I wasn't sure how to specify a composite primary key with `create-table` and ended up diving into its [code](https://github.com/clojure/clojure-contrib/blob/b8d2743d3a89e13fc9deb2844ca2167b34aaa9b6/src/main/clojure/clojure/contrib/sql.clj#L103).

``` clojure
(defn create-table
  "Creates a table on the open database connection given a table name and
  specs. Each spec is either a column spec: a vector containing a column
  name and optionally a type and other constraints, or a table-level
  constraint: a vector containing words that express the constraint. All
  words used to describe the table may be supplied as strings or keywords."
  [name & specs]
  (do-commands
   (format "CREATE TABLE %s (%s)"
           (as-str name)
           (apply str
                  (map as-str
                       (apply concat
                              (interpose [", "]
                                         (map (partial interpose " ") specs))))))))
```

Looking at `create-table` we can see it creates a SQL statement which is then executed by `do-commands`.
In order to have a composite key we need `do-commands` to execute a SQL statement that looks similar to below.

``` sql
CREATE TABLE track(
  date date,
  id integer,
  symbol char(10),
  price integer,
  quantity integer,
  PRIMARY KEY (date, id)
)
```

Let's break down `create-table` to figure out what we need to pass it to make `do-commands` run the above statement.
The code for `create-table` is repeated below with comments pointing out what step lines up the code.

``` clojure
(defn create-table
  [name & specs]
  (do-commands                                              ; step 7
   (format "CREATE TABLE %s (%s)"                           ; step 6
           (as-str name)
           (apply str                                       ; step 5
             (map as-str                                    ; step 4
              (apply concat                                 ; step 3
               (interpose [", "]                            ; step 2
                (map (partial interpose " ") specs))))))))  ; step 1
```

1. First `create-table` takes the sequences in `specs` and puts a space between each element in each sequence.
2. The result of step 1 then has a vector containing a comma and a space interposed between each element of it.
3. `concat` combined with `apply` is used to combine each element of the result of step 2 into a single sequence.
4. `as-str` (from [c.c.string](http://clojure.github.com/clojure-contrib/string-api.html#clojure.contrib.string/as-str)) is mapped over the result of step 3 to make sure every element is a string.
5. `str` is used to make one string out of the sequence of strings from step 4.
6. `format` is used to substitute in `name` and the result of step 5 to create the SQL statement.
7. `do-commands` executes the statement created in step 6.

Knowing how `create-table` works now allows us to specify the arguments that will create the orders table with the composite primary key of _date_ and _id_.

``` clojure
(create-table "orders"
              [:date     "date"]
              [:id       "integer"]
              [:symbol   "char(10)"]
              [:price    "integer"]
              [:quantity "integer"]
              ["PRIMARY KEY" "(date, id)")
```