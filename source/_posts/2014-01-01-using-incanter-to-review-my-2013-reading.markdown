---
layout: post
title: "Using Incanter to review my 2013 reading"
date: 2014-01-01 17:31
comments: true
categories: [books, incanter, clojure]
---

I've been using [goodreads](https://goodreads.com) to keep track of my
reading since 2010 (my
[profile](https://www.goodreads.com/user/show/3431614-jake-mccrary)). I've found it extremely useful for capturing what books I want to read along with being a repository of what I've read. I thought it would be fun to take a closer look at the books I read in 2013 and decided to use the [Clojure](http://clojure.org/) library [Incanter](http://incanter.org/). I haven't used Incanter since I [contributed](http://jakemccrary.com/blog/2010/02/21/plotting-time-series-data-with-incanter/) to it back in 2010 and thought this would be a good opportunity to regain some familiarity.

The first step to playing around with my goodreads data was to somehow get it out of goodreads. I've done work with the Goodreads API before [^1] but decided that was a bit overkill for this. Instead I used goodreads export functionality (follow the links: My Books > import/export) to download a csv file. This also let me do a little of data cleanup since some of the book's page counts were missing [^2].

[^1]: A project on Heroku that takes your to-read list from goodreads and queries the Chicago Public Library to see if books are available. Someday I'll give it some love and make it usable by others.

[^2]: I've also applied to be a goodreads librarian so I can actually fix their data as well.

Armed with my goodreads data it was time to start playing with it. After running `lein new goodreads-summary` I edited my `project.clj` file to have a dependency on Incanter.

``` clojure
    (defproject goodreads-summary "0.1.0-SNAPSHOT"
      :dependencies [[org.clojure/clojure "1.5.1"]
                     [org.clojure/data.csv "0.1.2"]
                     [incanter "1.5.4"]
                     [clj-time "0.6.0"]])
```

This first step towards playing with my data was to get it into an Incanter dataset. Since the data was already in a csv file it was really straightforward to do. Just needed to `require` the right libraries and use [incanter.io/read-dataset](http://liebke.github.io/incanter/io-api.html#incanter.io/read-dataset). The snippet below has all of the necessary requires for the remainder of the examples. It isn't well documented but by passing `:keyword-headers false` to `read-dataset` the headers are not converted to keywords. Some of the goodreads headers have spaces in them which makes dealing with them as keywords a pain.

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

Calling `read-csv` with the path to the exported goodreads data results in an Incanter dataset. A useful function when experimenting with datasets is [incanter.core/view](http://liebke.github.io/incanter/core-api.html#incanter.core/view). Running `(incanter/view (read-csv "goodreads_export.csv"))` pops up a grid of all of the entire dataset. For my purposes I didn't care about most of the data provided; I only wanted to look at a few columns and only books finished in 2013. This lead me to look into how to filter and select from an Incanter dataset.

Let's take a at selecting columns first. Selecting columns can be done with (incanter.core/sel)[http://liebke.github.io/incanter/core-api.html#incanter.core/sel]. Like most Incanter functions it has many overloaded versions. One way to use it is to pass a dataset as the first argument and then a vector of columns you want to select. 

``` clojure
    (defn select-columns [dataset]
      (incanter/sel dataset :cols (str->keys ["Number of Pages" "Date Read" "Bookshelves" "Exclusive Shelf"])))
```

Filtering a dataset is done using (incanter.core/$where)[]. Filtering for books that are on your goodreads *read* shelf is pretty straightforward.

``` clojure
    (defn finished [dataset]
      (incanter/$where {"Exclusive Shelf" "read"} dataset))
```

Filtering for books read in 2013 is a bit more complicated. First I converted the **"Date Read"** column from a string to a `org.joda.time.DateTime`. This was done with the combination of `transform-date-read-column` and `parse-date`. Some of my goodreads data doesn't have the **"Date Read"** column filled in. I handled this by replacing the missing date with a date time set to `clj-time.core/date-time 0`.

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
        (incanter/$where {"Date Read" {:fn (date-greater-than? (parse-date "2012/12/31"))}} with-date-objects)))
```

The `$where` in `books-read-in-2013` is a bit more complicated than the filtering done in `finished`. Here I'm providing a predicate to use instead of just doing an equality comparison.

After performing this filter I finally had that I read in 2013. Now to generate some data for each month. I approached this by writing a function that adds a **Month** column to my dataset. I originally had the function below. It uses `incanter.core/$map` to generate the data, makes a dataset with the new data, and then adds that to the original dataset.

``` clojure
    (defn add-month-read-column [dataset]
      (let [month-read (incanter/$map tc/month "Date Read" dataset)
            month-dataset (incanter/dataset [:month-read] month-read)
            with-month-read (incanter/conj-cols dataset month-dataset)]
        with-month-read))
```

This felt like a indirect way of adding a derived column. While writing this post I happened to run across (incanter.core/add-derived-column)[] which does exactly what we want. The above function turned into a one-liner.

``` clojure
    (defn add-month-read-column [dataset]
      (incanter/add-derived-column :month-read ["Date Read"] tc/month dataset))
```

With `add-month-read-column` in place we can now start aggregating some stats. Below is code for figuring out the total number of pages read per month and the number of books read.

``` clojure
    (defn pages-by-month [dataset]
      (let [with-month-read (add-month-read-column dataset)]
        (->> (incanter/$rollup :sum "Number of Pages" :month-read with-month-read)
             (incanter/$order :month-read :asc))))
    
    (defn book-count-by-month [dataset]
      (let [with-month-read (add-month-read-column dataset)]
        (->> (incanter/$rollup :count "Number of books" :month-read with-month-read)
             (incanter/$order :month-read :asc))))

    (defn stats-by-month [dataset]
      (->> (incanter/$join [:month-read :month-read]
                         (pages-by-month dataset)
                         (book-count-by-month dataset))
           (incanter/rename-cols {:month-read "Month"
                                "Number of Pages" "Page Count"
                                "Number of books" "Book Count"})
           (incanter/add-derived-column "Pages/Books"
                                      ["Page Count" "Book Count"]
                                      (fn [p b] (Math/round (double (/ p b)))))))
```

`pages-by-month` and `book-count-by-month` are very similar. Each uses (incanter.core/$rollup)[http://liebke.github.io/incanter/core-api.html#incanter.core/$rollup] to calculate per month stats. The first argument to `$rollup` can be a function that takes a sequence of values or one of the supported magical "function identifier keywords".

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

So a nice ascii table is great but what about producing some images? Incanter has some functions for doing just that. Lets make a bar chart.

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

Running the snippet `view-page-count-chart` produces a pop-up with the below bar chart. The chart actually surprises me as I fully expected to have higher page counts during the winter months than the summer months.

![Bar chart of total page count by month](/images/page-count-by-month.png "Bar chart of total page count by month")

