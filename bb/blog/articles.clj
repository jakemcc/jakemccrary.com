(ns blog.articles
  (:require [babashka.fs :as fs]
            [blog.render]
            [clojure.java.io]
            [clojure.string]) 
  (:import [java.time LocalDate ZoneId ZonedDateTime]
           [java.time.format DateTimeFormatter DateTimeParseException]))

(defn- publish-draft [source]
  (println "Publishing" (-> source :metadata :title))
  (let [lines (clojure.string/split-lines (slurp (:input-file source)))]
    (with-open [w (clojure.java.io/writer (:input-file source))]
      (binding [*out* w]
        (doseq [line lines]
          (cond
            (= line "published: false")
            (println "published: true")

            (clojure.string/starts-with? line "date: ")
            (println "date:" (blog.render/rfc-3339-now))

            :else
            (println line)))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn publish [& _args]
  (let [unpublished (filterv (fn [source] (false? (-> source :metadata :published)))
                             (blog.render/load-sources true))]
    (println "Select a draft to publish:")
    (doseq [[i s] (map vector (range) unpublished)]
      (println (format "%s) %s" i (-> s :metadata :title))))
    (print "Enter a number: ")
    (flush)
    (let [selection (read)]
      (when-not (<= 0 selection (count unpublished))
        (println "Must pick a valid option")
        (System/exit 1))
      (publish-draft (first (drop selection unpublished))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn list-drafts [& _args]
  (println "Listing all draft posts:")
  (println "------------------------")
  (doseq [draft (filter (fn [source]
                         (false? (-> source :metadata :published)))
                       (blog.render/load-sources true))
          :let [title (-> draft :metadata :title)
                date (-> draft :metadata :local-date)]]
    (if date
      (println (format "%s (dated: %s)" 
                      title 
                      (blog.render/date->human-readable date)))
      (println title))))

(defn- slugify [title]
  (-> title
      clojure.string/lower-case
      (clojure.string/replace #"\s+" "-")
      (clojure.string/replace #"[^a-z0-9-]" "")))

(defn- prompt-title []
  (print "Enter title: ")
  (flush)
  (let [title (read-line)]
    (when-not (clojure.string/blank? title)
      title)))

(defn- create-content [metadata]
  (str "---\n"
       (clojure.string/join "\n"
         (for [[k v] metadata]
           (if (sequential? v)
             (str k ":\n" (clojure.string/join "\n" (map #(str "- " %) v)))
             (str k ": " v))))
       "\n---\n\n"
       "TODO: Write content\n"))

(defn- create-new-file [{:keys [type filename metadata]}]
  (let [file-path (fs/file blog.render/source-dir type filename)]
    (fs/create-dirs (fs/parent file-path))
    (spit file-path (create-content metadata))
    (println (str "Created new " type ": " file-path))))

(defn create-new-post [title]
  (let [now (ZonedDateTime/now (ZoneId/of "America/Chicago"))
        date-str (.format now (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm Z"))
        filename (str (blog.render/->yyyy-MM-dd now)
                     "-"
                     (slugify title)
                     ".markdown")]
    (create-new-file
     {:type "blog"
      :title title
      :date-str date-str
      :filename filename
      :metadata {"layout" "post"
                "title" title
                "date" date-str
                "comments" true
                "published" false
                "description" "TODO"
                "categories" ["TODO"]}})))

(defn create-new-adventure [title start-date end-date]
  (let [now (ZonedDateTime/now (ZoneId/of "America/Chicago"))
        date-str (.format now (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm Z"))
        start-date-str (blog.render/->yyyy-MM-dd start-date)
        end-date-str (blog.render/->yyyy-MM-dd end-date)
        filename (str start-date-str
                     "-"
                     end-date-str
                     "-"
                     (slugify title)
                     ".markdown")]
    (create-new-file
     {:type "adventures"
      :title title
      :date-str date-str
      :filename filename
      :metadata {"layout" "adventure"
                "title" title
                "date" date-str
                "start-date" start-date-str
                "end-date" end-date-str
                "published" false
                "description" "TODO"}})))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn new-post [& _args]
  (if-let [title (prompt-title)]
    (create-new-post title)
    (println "Title cannot be blank")))

(defn parse-date [date-str]
  (try
    (LocalDate/parse date-str (DateTimeFormatter/ofPattern "yyyy-MM-dd"))
    (catch DateTimeParseException _
      nil)))

(defn prompt-date [prompt]
  (print prompt)
  (flush)
  (let [input (read-line)]
    (if-let [date (parse-date input)]
      date
      (do
        (println "Invalid date format. Please use YYYY-MM-DD")
        (recur prompt)))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn new-adventure [& _args]
  (if-let [title (prompt-title)]
    (let [start-date (prompt-date "Enter start date (YYYY-MM-DD): ")
          end-date (prompt-date "Enter end date (YYYY-MM-DD): ")]
      (if (.isAfter start-date end-date)
        (println "Start date cannot be after end date")
        (create-new-adventure title start-date end-date)))
    (println "Title cannot be blank")))

