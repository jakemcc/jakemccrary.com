(ns blog.render
  (:require [babashka.fs :as fs]
            [babashka.process]
            [clojure.data.xml :as xml]
            [clojure.string]
            [hiccup2.core :as hiccup]
            [markdown.core :as markdown]
            [selmer.parser])
  (:import (java.time LocalDate
                      LocalDateTime
                      ZoneId
                      ZonedDateTime)
           (java.time.format DateTimeFormatter DateTimeParseException FormatStyle)
           [java.util Locale]))

(defn left-pad [s padding n]
  (if (< (count s) n)
    (recur (str padding s) padding n)
    s))

(defmacro dbg [& args]
  `(let [r# (do ~@args)]
     (println (str "dbg: " (quote ~@args) " => " (pr-str r#)))
     r#))

(def source-dir "posts")
(def output-dir "output")
(def template-dir "templates")
(def ^:dynamic *preview* false)

(def default-render-opts
  {:site {:title "Jake McCrary"
          :subscribe-email "https://jakemccrary.substack.com/welcome"
          :subscribe-rss "http://feeds.feedburner.com/JakeMccrarysMusings"
          :url "https://jakemccrary.com"
          :short-url "jakemccrary.com"}})

(defn output-file [source]
  (let [sub-dirs (->> (fs/components (:input-file source))
                      (rest)
                      (butlast))
        name (fs/strip-ext (fs/file-name (:input-file source)))
        year-month-day (when (-> source :metadata :local-date)
                         [(str (.getYear (-> source :metadata :local-date)))
                          (left-pad (str (.getMonthValue (-> source :metadata :local-date))) "0" 2)
                          (left-pad (str (.getDayOfMonth (-> source :metadata :local-date))) "0" 2)])]
    (apply fs/file (concat sub-dirs
                           (when (-> source :metadata :dated-url)
                             year-month-day)
                           [(cond-> name
                              (some (comp #{"blog"} str) sub-dirs)
                              (clojure.string/replace-first #"^\d\d\d\d-\d\d-\d\d-" ""))
                            "index.html"]))))
(comment
  (= "adventures/2022-10-14-2022-10-18-a-beautiful-fall-trip-to-the-red-river-gorge/index.html"
     (str (output-file {:metadata
                        {:layout "page",
                         :published true,
                         :title "A beautiful fall trip to the Red River Gorge",
                         :date "2022-11-27 18:01:46 -0600",
                         :start_date #inst "2022-10-14T00:00:00.000-00:00",
                         :end_date #inst "2022-10-18T00:00:00.000-00:00",
                         :description "A long weekend with Jenn climbing and camping in the Red River Gorge"},
                        :input-file (fs/file "posts/adventures/2022-10-14-2022-10-18-a-beautiful-fall-trip-to-the-red-river-gorge.markdown")})))
  (= "blog/reading-in-2024/index.html"
     (str (output-file {:input-file (fs/file "posts/blog/2024-01-05-reading-in-2024.markdown")
                        :metadata {:date "2025-01-05 16:23 -0600"
                                   :local-date (LocalDate/of 2025 1 5)}})))
  ;;
  )

(defn load-template [post]
  (let [template "default" #_ (or (-> post :metadata :layout) "default")
        file (fs/file template-dir (str template ".html"))]
    (if (fs/exists? file)
      (slurp file)
      (throw (Exception. (str "template not found: " (str file)))))))

(defn pre-process-markdown [markdown]
  (-> markdown
      (clojure.string/replace #"--" (fn [_] "$$NDASH$$"))
      (clojure.string/replace #"\[[^\]]+\n"
                   (fn [match]
                     (clojure.string/replace match "\n" "$$RET$$")))))

(defn post-process-markdown [html]
  (-> html
      (clojure.string/replace "$$NDASH$$" "--")
      (clojure.string/replace "$$RET$$" "\n")))

(def formatters
  [(DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm Z")   
   (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")     
   (DateTimeFormatter/ofPattern "yyyy-MM-dd")           
   (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss Z")
   (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")
   (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssZ")])

(defn parse-datetime [s]
  (let [parse (fn [f] (try
                        (LocalDateTime/parse s f)
                        (catch DateTimeParseException e
                          (try
                            (LocalDate/parse s f)
                            (catch DateTimeParseException e
                              nil)))))]
    (loop [[format & formatters] formatters]
      (if-not format
        (throw (Exception. (str "Could not parse " s)))
        (if-let [t (parse format)]
          t
          (recur formatters))))))

(defn ->yyyy-MM-dd [d]
  (.format d (DateTimeFormatter/ofPattern "yyyy-MM-dd")))

(defn date->human-readable [d]
  (.format d (.withLocale (DateTimeFormatter/ofLocalizedDate FormatStyle/LONG) Locale/ENGLISH)))

(defn post-process-metadata [source]
  (let [date (when (-> source :metadata :date)
               (let [d (-> source :metadata :date)
                     parsed (cond
                              (inst? d)
                              (LocalDateTime/ofInstant (.toInstant d) (ZoneId/of "America/Chicago"))
                              :else
                              (parse-datetime d))]
                 (if (instance? LocalDate parsed)
                   parsed
                   (.toLocalDate parsed))))
        source (cond-> source
                 (-> source :metadata :start_date)
                 (update-in [:metadata :start_date] #(.toLocalDate (.atZone (.toInstant %) (ZoneId/of "UTC"))))
                 (-> source :metadata :end_date)
                 (update-in [:metadata :end_date] #(.toLocalDate (.atZone (.toInstant %) (ZoneId/of "UTC")))))]
    (cond-> source
      date
      (-> (assoc-in [:metadata :local-date] date)
          (assoc-in [:metadata :published-yyyymmdd] (->yyyy-MM-dd date))
          (assoc-in [:metadata :published-readable] (date->human-readable date)))
      (-> source :metadata :start_date)
      (assoc-in [:metadata :start-date-readable] (date->human-readable (-> source :metadata :start_date)))
      (-> source :metadata :end_date)
      (assoc-in [:metadata :end-date-readable] (date->human-readable (-> source :metadata :end_date)))
      (-> source :metadata :categories)
      (update-in [:metadata :categories]
                 #(into #{} (mapv clojure.string/lower-case %))))))

(defn markdown->source [file]
  (let [markdown (slurp (fs/file file))]
    (println "Processing markdown for file:" (str file))
    (-> markdown
        ;; pre-process-markdown
        (markdown/md-to-html-string-with-meta :reference-links? true
                                              :footnotes? true
                                              :code-style
                                              (fn [lang]
                                                (format "class=\"language-%s\"" lang)))
        post-process-metadata
        #_#_        :html
        post-process-markdown)))

(defn load-sources []
  (when (fs/exists? source-dir)
    (for [path (fs/glob source-dir "**.markdown")
          :let [source (as-> (markdown->source path) source
                         (assoc source :input-file (fs/file path))
                         (assoc source :output-file (output-file source))
                         (assoc source :template (load-template source)))]
          :when (or *preview*
                    (or (not (contains? (:metadata source) :published))
                        (-> source :metadata :published)))]
      source)))

(defn blog-article? [{:keys [input-file]}]
  (clojure.string/includes? (str input-file) "/blog/"))

(defn adventure? [{:keys [input-file]}]
  (clojure.string/includes? (str input-file) "/adventures/"))

(defn blog-articles [sources]
  (sort-by (comp :local-date :metadata)
           (filter blog-article? sources)))

(defn write-html! [output-file m]
  (fs/create-dirs (fs/parent output-file))
  (when (fs/exists? output-file)
    (throw (Exception. (str "Tried to write file but it already exists:" output-file))))
  (spit output-file
        (selmer.parser/render
         (or (:template m) (slurp (fs/file template-dir "default.html")))
         (merge default-render-opts m))))

(defn write-post! [source]
  (when-not (clojure.string/blank? (:html source))
    (let [out-file (fs/file output-dir (:output-file source))]
      (fs/create-dirs (fs/parent out-file))
      (write-html! out-file
                   {:body (:html source)
                    :page (:metadata source)
                    :template (:template source)}))))

(defn blog-url
  ([path] (blog-url "" path))
  ([root path]
   (-> path
       (clojure.string/replace "index.html" "")
       (clojure.string/replace-first #"^/" "")
       (clojure.string/replace #"/$" "")
       (str "/")
       (->> (str root "/")))))

(defn- article-list [articles]
  [:ul {:class "post-list"}
   (for [article articles
         :let [metadata (:metadata article)]]
     [:li
      [:div.post-meta
       [:time (str (:local-date metadata))]
       [:h2 [:a {:href (blog-url (:output-file article))} (:title metadata)]]]
      (when-not (clojure.string/blank? (:description metadata))
        [:p.post-description (:description metadata)])])])

(defn write-index! [sources]
  (write-html! (fs/file output-dir "index.html")
               {:body (hiccup/html
                       [:div (article-list (reverse (blog-articles sources)))])}))

(defn write-archive! [sources]
  (let [articles (reverse (blog-articles sources))]
    (write-html! (fs/file output-dir "blog" "archives" "index.html")
                 {:body (hiccup/html
                         [:div {:id "blog-archives"}
                          (article-list articles)])})))

(xml/alias-uri 'atom "http://www.w3.org/2005/Atom")

(defn- rfc-3339-now []
  (let [fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssxxx")
        now (ZonedDateTime/now java.time.ZoneOffset/UTC)]
    (.format now fmt)))

(defn- rfc-3339 [local-date]
  (let [fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssxxx")
        now (ZonedDateTime/of (.atTime local-date 23 59 59) java.time.ZoneOffset/UTC)]
    (.format now fmt)))

(defn- atom-feed
  ;; validate at https://validator.w3.org/feed/check.cgi
  ([articles] (atom-feed nil articles))
  ([category articles]
   (let [blog-root "https://jakemccrary.com/"]
     (-> (xml/sexp-as-element
          [::atom/feed
           {:xmlns "http://www.w3.org/2005/Atom"}
           [::atom/title [:-cdata (str "Jake McCrary's articles"
                                       (when category
                                         (str " on " category)))]]
           [::atom/link {:href "https://jakemccrary.com/atom.xml" :rel "self"}]
           [::atom/link {:href blog-root}]
           [::atom/updated (rfc-3339-now)]
           [::atom/id blog-root]
           [::atom/author
            [::atom/name [:-cdata "Jake McCrary"]]]
           (for [article articles
                 :let [output-file (:output-file article)
                       {:keys [title local-date published]} (:metadata article)]
                 :when published
                 :let [link (str blog-root (str output-file))]]
             [::atom/entry
              [::atom/id link]
              [::atom/link {:href link}]
              [::atom/title [:-cdata title]]
              [::atom/updated (rfc-3339 local-date)]
              [::atom/content {:type "html"}
               [:-cdata (:html article)]]])])
         xml/indent-str))))

(defn- copy-resources []
  (doseq [f (fs/glob (fs/file source-dir) "**.{css,png,gif,jpeg,jpg,svg,js,webm,mp4}")
          :let [out (apply fs/file output-dir (rest (fs/components f)))]]
    (fs/create-dirs (fs/parent out))
    (fs/copy f out)))

(defn- write-main-feed! [sources]
  (spit (fs/file output-dir "atom.xml")
        (atom-feed (reverse (blog-articles sources)))))

(defn- write-category-page! [category articles]
  (write-html! (fs/file output-dir "blog" "categories" category "index.html")
               {:body (hiccup/html
                       [:div
                        [:h2 "Category: " category]
                        [:p [:a {:href (str "/blog/categories/" category "/atom.xml")}
                             "RSS feed"]]
                        (article-list articles)])})
  (spit (fs/file output-dir "blog" "categories" category "atom.xml")
        (atom-feed category articles)))

(defn- write-category-pages! [sources]
  (let [articles (reverse (blog-articles sources))
        categories (into #{}
                         (comp (map :metadata) (mapcat :categories))
                         articles)]
    (doseq [category categories]
      (write-category-page! category
                            (filter #(contains? (-> % :metadata :categories) category)
                                    articles)))))

(defn- adventure-list [articles]
  [:ul {:class "post-list"}
   (for [article articles
         :let [metadata (:metadata article)]]
     [:li
      [:div.post-meta
       [:h2 (if (clojure.string/blank? (:html article))
              (:title metadata)
              [:a {:href (blog-url (:output-file article))} (:title metadata)])]
       [:div (clojure.string/join " "
                                  (cons (->yyyy-MM-dd (:start_date metadata))
                                        (when (:end_date metadata)
                                          ["to" (->yyyy-MM-dd (:end_date metadata))])))]]
      (when-not (clojure.string/blank? (:description metadata))
        [:p.post-description (:description metadata)])])])

(defn- write-adventures! [sources]
  (let [adventures (->> sources
                        (filterv adventure?)
                        (sort-by (comp :start_date :metadata))
                        reverse)]
    (write-html! (fs/file output-dir "adventures" "index.html")
                 {:body (hiccup/html
                         (adventure-list adventures))})))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}

(defn render [{:keys [preview] :as args}]
  (println "Rendering " args)
  (if (fs/exists? output-dir)
    (run! fs/delete-tree (fs/list-dir output-dir))
    (fs/create-dir output-dir))

  (binding [*preview* (boolean preview)]
    (let [sources (load-sources)]
      (copy-resources)
      (run! write-post! sources)
      (write-index! sources)
      (write-archive! sources)
      (write-adventures! sources)
      (write-category-pages! sources)
      (write-main-feed! sources))))

(comment
  (def sources (doall (load-sources)))
  (def articles (blog-articles sources))
  (def adventures (filterv adventure? sources))
  (->yyyy-MM-dd (:start_date (:metadata (first adventures))))

  (:metadata (last articles))
  (def r *1)

  (write-adventures! sources)
  (type (:start_date (:metadata (first adventures))))

  (mapv (comp :title :metadata)
        (filterv (fn [{:keys [metadata]}]
                   (some char? (:categories metadata)))
                 articles))
  (into #{} (mapcat (comp :categories :metadata)) articles)

  (->> (mapv (juxt #(get-in % [:metadata :local-date])
                   #(get-in % [:metadata :date]))
             articles)
       (sort-by first))

;;
  )

