---
layout: post
title: "Using Incanter to review my 2013 reading"
date: 2014-01-01 17:31
comments: true
categories: [books, incanter, clojure]
---

I use [goodreads](https://goodreads.com) to keep track of
[my reading](https://www.goodreads.com/user/show/3431614-jake-mccrary)
and have since early 2010. I find very useful for capturing what I
want to read and reminding me of how I felt about books I've read. I
thought it would be fun to take a closer look at what I read in 2013.
I'm doing this using [Clojure](http://clojure.org/) with
[Incanter](http://incanter.org/). I haven't used Incanter since I
wrote
[this](http://jakemccrary.com/blog/2010/02/21/plotting-time-series-data-with-incanter/)
post and thought this would be a good opportunity to visit it again.

First I need to get my data out of goodreads. I've worked with the
Goodreads API before [^1] but am not going to use it for this
exercise. Instead I'm using the goodreads export functionality (at
goodreads follow the links: My Books > import/export) to export a csv
file. Having the csv file also lets me cleanup some of the data since
some of the book's page counts were missing [^2].

[^1]: A project on Heroku that takes your to-read list from goodreads and queries the Chicago Public Library to see if books are available. Someday I'll give it some love and make it usable by others.

[^2]: I've also applied to be a goodreads librarian so I can actually fix their data as well.

Now that I have data it is time to start playing with it. Run `lein new goodreads-summary` and edit the `project.clj` file to have a dependency on Incanter.

``` clojure
    (defproject goodreads-summary "0.1.0-SNAPSHOT"
      :dependencies [[org.clojure/clojure "1.5.1"]
                     [org.clojure/data.csv "0.1.2"]
                     [incanter "1.5.4"]
                     [clj-time "0.6.0"]])
```

Next I'm going to take the csv file and transform it into an Incanter
dataset. This is easily done with
[incanter.io/read-dataset](http://liebke.github.io/incanter/io-api.html#incanter.io/read-dataset).
It isn't well documented but by passing `:keyword-headers false` to
`read-dataset` the headers from the csv are not converted to keywords.
I'm doing this because some of the goodreads headers contain spaces
and dealing with spaces in keywords is a pain. The snippet below has
all of the necessary requires for the remainder of the examples.

``` clojure
    (ns goodreads-summary.core
      (:require [clojure.data.csv :as csv]
                [clojure.string :as str]
                [incanter.core :as incanter]
                [incanter.io :as io]
                [incanter.charts :as charts]
                [clj-time.core :as tc]
                [clj-time.format :as tf]))
    
    (defn read-csv [filepath]
      (io/read-dataset filepath :header true :keyword-headers false))
```

Calling `read-csv` with the path to the exported goodreads data
results in dataset. If you want to view the data use
[incanter.core/view](http://liebke.github.io/incanter/core-api.html#incanter.core/view).
Running `(incanter/view (read-csv "goodreads_export.csv"))` pops up a
grid of with all the data. I don't care about most of the columns so
lets define a function that selects out the few I care about.

``` clojure
    (defn select-columns [dataset]
      (incanter/sel dataset :cols ["Number of Pages" "Date Read" "Bookshelves" "Exclusive Shelf"]))
```

Selecting columns is done with
[incanter.core/sel](http://liebke.github.io/incanter/core-api.html#incanter.core/sel).
Like most Incanter functions it has many overloads. One way to use it
is to pass a dataset with a vector of columns you want to select.


Filtering a dataset is done using
[incanter.core/$where](http://liebke.github.io/incanter/core-api.html#incanter.core/$where).
Goodreads has three default shelves **to-read**,
**currently-reading**, and **read**. To select all your read books you
filter of the **Exclusive Shelf** column for **read** books.

``` clojure
    (defn finished [dataset]
      (incanter/$where {"Exclusive Shelf" "read"} dataset))
```

Filtering for books read in 2013 is a bit more complicated. First I
convert the **Date Read** column from a string to a
`org.joda.time.DateTime`. This is done with the combination of
`transform-date-read-column` and `parse-date`. Some of the my data is missing a **Date Read** value. I'm choosing to handle this by treating missing data as the result of `(clj-time.core/date-time 0)`.

The `$where` in `books-read-in-2013` is a bit more complicated than
the filtering in `finished`. Here I'm providing a predicate to
use instead of just doing an equality comparison.

``` clojure
    (defn parse-date [date-str]
      (if date-str
        (tf/parse (tf/formatter "yyyy/MM/dd") date-str)
        (tc/date-time 0)))
    
    (defn transform-date-read-column [dataset]
      (incanter/transform-col dataset "Date Read" parse-date))
    
    (defn date-greater-than-pred [date]
      (fn [challenger]
        (> (.compareTo challenger date) 0)))
    
    (defn books-read-in-2013 [dataset]
      (let [finished (finished (select-columns dataset))
            with-dates (incanter/$where {"Date Read" {:fn identity}} finished)
            with-date-objects (transform-date-read-column with-dates)]
        (incanter/$where {"Date Read" {:fn (date-greater-than-pred (parse-date "2012/12/31"))}}
                         with-date-objects)))
```

Now we have a dataset that that contains only books read in 2013
(well, until I read a book in 2014 and the filter above also grabs
books in 2014). Now to generate some analytic for each month. First
lets add a **Month** column to our data. Originally I wrote the
function below. It uses `incanter.core/$map` to generate the data,
makes a dataset with the new data, and then adds that to the original
dataset.

``` clojure
    (defn add-month-read-column [dataset]
      (let [month-read (incanter/$map tc/month "Date Read" dataset)
            month-dataset (incanter/dataset ["Month"] month-read)
            with-month-read (incanter/conj-cols dataset month-dataset)]
        with-month-read))
```

When I wrote the above code it seemed like there should be a better
way. While writing this post I stumbled across
[incanter.core/add-derived-column](http://liebke.github.io/incanter/core-api.html#incanter.core/add-derived-column).
Switching to `add-derived-column` makes `add-month-read-column` almost trivial.

``` clojure
    (defn add-month-read-column [dataset]
      (incanter/add-derived-column "Month" ["Date Read"] tc/month dataset))
```

Now that we have `add-month-read-column`  we can now start aggregating
some stats. Lets write code for calculating the pages read per month.

``` clojure
    (defn pages-by-month [dataset]
      (let [with-month-read (add-month-read-column dataset)]
        (->> (incanter/$rollup :sum "Number of Pages" "Month" with-month-read)
             (incanter/$order "Month" :asc))))
```

That was pretty easy. Lets write a function to count the number of books read per month.

``` clojure
    (defn book-count-by-month [dataset]
      (let [with-month-read (add-month-read-column dataset)]
        (->> (incanter/$rollup :count "Number of books" "Month" with-month-read)
             (incanter/$order "Month" :asc))))
```

`pages-by-month` and `book-count-by-month` are very similar. Each uses [incanter.core/$rollup](http://liebke.github.io/incanter/core-api.html#incanter.core/$rollup) to calculate per month stats. The first argument to `$rollup` can be a function that takes a sequence of values or one of the supported magical "function identifier keywords".

Next lets combine the data together so we can print out a nice table. While we are at it lets add another column.

``` clojure
    (defn stats-by-month [dataset]
      (->> (incanter/$join ["Month" "Month"]
                         (pages-by-month dataset)
                         (book-count-by-month dataset))
           (incanter/rename-cols {"Number of Pages" "Page Count"
                                  "Number of books" "Book Count"})
           (incanter/add-derived-column "Pages/Books"
                                      ["Page Count" "Book Count"]
                                      (fn [p b] (Math/round (double (/ p b)))))))
```

`stats-by-month` returns a dataset which when printed looks like the following table. It joins the data, renames columns, and adds a derived column.


    | Month | Book Count | Page Count | Pages/Books |
    |-------+------------+------------+-------------|
    |     1 |          6 |       1279 |         213 |
    |     2 |          2 |       1251 |         626 |
    |     3 |          8 |       2449 |         306 |
    |     4 |          5 |       1667 |         333 |
    |     5 |          6 |       2447 |         408 |
    |     6 |          5 |       1609 |         322 |
    |     7 |          5 |       1445 |         289 |
    |     8 |          5 |       2229 |         446 |
    |     9 |          2 |        963 |         482 |
    |    10 |          5 |       1202 |         240 |
    |    11 |          5 |       2248 |         450 |
    |    12 |          7 |       1716 |         245 |

Great. Now we have a little ascii table. Lets get graphical and make some bar charts.

``` clojure
    (defn chart-column-by-month [column dataset]
      (let [select (fn [column] (incanter/sel dataset :cols column))
            months (select "Month")]
        (charts/bar-chart months (select column)
                          :y-label column :x-label "Month")))
    
    (defn chart-page-count-by-month [dataset]
      (chart-column-by-month "Page Count" dataset))
    
    (defn chart-book-count-by-month [dataset]
      (chart-column-by-month "Book Count" dataset))

    (defn view-page-count-chart []
      (-> (read-csv "goodreads_export.csv")
          books-read-in-2013
          stats-by-month
          chart-page-count-by-month
          incanter/view))
```

Running the snippet `view-page-count-chart` produces a pop-up with the
below bar chart. The chart actually surprises me as I fully expected
to have higher page counts during the winter months than the summer
months. This chart and analysis is pretty useless though without
knowing the difficulty of the pages read. For example, last February I
read
[Infinite Jest](http://www.amazon.com/Infinite-Jest-David-Foster-Wallace/dp/0316920045/).
Knowing that I don't feel like having a low page count in that month
is slacking at all.

![Bar chart of total page count by month](/images/page-count-by-month.png "Bar chart of total page count by month")


### 2013 Summary ###

2013 was a pretty big year of reading. I read more books this past
year than all other years that I have data. I also read some of the
best books I've ever read. Not only that but I actually created
multiple [^3] custom Kindle dictionaries to help improve my (and
others) reading experience.

[^3]: One for [Functional JavaScript](http://jakemccrary.com/blog/2013/07/09/releasing-the-functional-javascript-companion/) and another for [Dune](http://gum.co/dune-dictionary). If you want a custom Kindle dictionary made feel free to reach out.

Summary table [^4]:

[^4]: tech shelf only includes programming books.

    |   :shelf | :books | :pages |
    |----------+--------+--------|
    | non-tech |     51 |  17798 |
    |     tech |     10 |   2707 |
    |     read |     61 |  20505 |
    


### Plans for 2014 ###

I'm planning on reading a similar amount in this upcoming year but
will probably have a bit more non-fiction books. First step towards doing
that is to start classifying my books as non-fiction or fiction. I'm
also planning on rereading at least two books that I've read in the
last few years. This is unusual for me because I don't often reread
books that quickly.

If you have any book recommendations feel free to leave them in the
comments or contact me through twitter or email.
