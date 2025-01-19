---
dated-url: true
layout: post
title: "Reading from a Zipfile in Clojure"
date: 2019-04-22 22:14:24 -0500
comments: true
published: false
description: PUT SUMMARY HERE 
keywords: 'csv, keywords, here'
categories: 
---

```clojure
(defn entry [zipfile-stream filename]
  (->> (repeatedly #(.getNextEntry zipfile-stream))
       (take-while identity)
       (drop-while #(not= (.getName %) filename))
       first))

(defn read-file
  [path path-in-zip]
  (with-open [zipfile (ZipInputStream. (io/input-stream path ; maybe?
                                                        ))]
    (let [entry (entry zipfile path)
          length (.getSize entry)
          buf (byte-array length)]
      (loop [offset 0]
        (let [bytes-read (.read zipfile buf offset (- length offset))]
          (when (and (not= bytes-read -1)
                     (< (+ offset bytes-read) length))
            (recur (+ offset bytes-read)))))
      (with-open (r (io/reader buf))
        (doall (json/parse-stream r keyword))))))

```
