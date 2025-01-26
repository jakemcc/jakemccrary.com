(ns blog.render
  (:require [babashka.fs :as fs]
            [babashka.process]
            [cheshire.core]
            [clojure.data.xml :as xml]
            [clojure.string]
            [clojure.set]
            [hiccup2.core :as hiccup]
            [markdown.core :as markdown]
            [selmer.parser]
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske])
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

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro dbg [& args]
  `(let [r# (do ~@args)]
     (println (str "dbg: " (quote ~@args) " => " (pr-str r#)))
     r#))

(def blog-root "https://jakemccrary.com/")
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
                           (if (:as-html-page (:metadata source))
                             [(str name ".html")]
                             [(cond-> name
                                (some (comp #{"blog"} str) sub-dirs)
                                (clojure.string/replace-first #"^\d\d\d\d-\d\d-\d\d-" ""))
                              "index.html"])))))
(comment
  [(= "404.html"
      (str (output-file {:metadata  {:layout "page", :title "Not found", :as-html-page true}
                         :html "<p>:(</p>"
                         :input-file (fs/file "posts" "404.markdown")})))

   (= "adventures/2022-10-14-2022-10-18-a-beautiful-fall-trip-to-the-red-river-gorge/index.html"
      (str (output-file {:metadata
                         {:layout "page",
                          :published true,
                          :title "A beautiful fall trip to the Red River Gorge",
                          :date "2022-11-27 18:01:46 -0600",
                          :start-date #inst "2022-10-14T00:00:00.000-00:00",
                          :end-date #inst "2022-10-18T00:00:00.000-00:00",
                          :description "A long weekend with Jenn climbing and camping in the Red River Gorge"},
                         :input-file (fs/file "posts/adventures/2022-10-14-2022-10-18-a-beautiful-fall-trip-to-the-red-river-gorge.markdown")})))
   (= "blog/reading-in-2024/index.html"
      (str (output-file {:input-file (fs/file "posts/blog/2024-01-05-reading-in-2024.markdown")
                         :metadata {:date "2025-01-05 16:23 -0600"
                                    :local-date (LocalDate/of 2025 1 5)}})))]
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
                              (LocalDateTime/ofInstant (.toInstant d) (ZoneId/of "UTC"))
                              :else
                              (parse-datetime d))]
                 (if (instance? LocalDate parsed)
                   parsed
                   (.toLocalDate parsed))))
        source (cond-> source
                 (-> source :metadata :start-date)
                 (update-in [:metadata :start-date] #(.toLocalDate (.atZone (.toInstant %) (ZoneId/of "UTC"))))
                 (-> source :metadata :end-date)
                 (update-in [:metadata :end-date] #(.toLocalDate (.atZone (.toInstant %) (ZoneId/of "UTC"))))
                 (-> source :metadata :updated)
                 (update-in [:metadata :updated] #(.toLocalDate (.atZone (.toInstant %) (ZoneId/of "UTC")))))]
    (cond-> source
      date
      (-> (assoc-in [:metadata :local-date] date)
          (assoc-in [:metadata :published-yyyymmdd] (->yyyy-MM-dd date))
          (assoc-in [:metadata :published-readable] (date->human-readable date)))
      (-> source :metadata :start-date)
      (assoc-in [:metadata :start-date-readable] (date->human-readable (-> source :metadata :start-date)))
      (-> source :metadata :end-date)
      (assoc-in [:metadata :end-date-readable] (date->human-readable (-> source :metadata :end-date)))
      (-> source :metadata :categories)
      (update-in [:metadata :categories]
                 (fn [categories]
                   (into #{}
                         (comp
                          (map clojure.string/lower-case)
                          (map #(clojure.string/replace % " " "-")))
                         categories))))))

(defn markdown->source [file]
  (let [markdown (slurp (fs/file file))]
    ;; (println "Processing markdown for file:" (str file))
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

(defn- categories [sources]
  (disj (into #{}
              (comp (map :metadata)
                    (mapcat :categories))
              sources)
        nil))

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
            [::atom/link {:href link}] ;;TODO: check this
            [::atom/title [:-cdata title]]
            [::atom/updated (rfc-3339 local-date)]
            [::atom/content {:type "html"}
             [:-cdata (:html article)]]])])
       xml/indent-str)))

(defn- sitemap
  [sources]
  (-> (xml/sexp-as-element
       [:urlset
        {"xmlns" "http://www.sitemaps.org/schemas/sitemap/0.9"
         "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"
         "xsi:schemaLocation" "http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd"}
        (concat (for [source sources
                      :when (not (clojure.string/blank? (:html source)))]
                  [:url
                   [:loc (str blog-root (str (:output-file source)))]
                   (when (-> source :metadata :local-date)
                     [:lastmod (rfc-3339 (or (-> source :metadata :updated)
                                             (-> source :metadata :local-date)))])])
                (for [category (categories sources)]
                  [:url [:loc (str blog-root "/blog/categories/" category "/")]])
                [[:url
                  [:loc blog-root]
                  [:lastmod (rfc-3339-now)]]])])
      xml/indent-str))

(defn- write-sitemap! [sources]
  (spit (fs/file output-dir "sitemap.xml") (sitemap sources)))

(defn- copy-resources []
  (doseq [f (->> (fs/glob (fs/file source-dir) "**" {:hidden true})
                 (remove (fn [path] (= "markdown" (fs/extension path))))
                 (remove (fn [path] (= ".DS_Store" (fs/file-name path)))))
          :let [out (apply fs/file output-dir (rest (fs/components f)))]]
    (fs/create-dirs (fs/parent out))
    (fs/copy f out)))

(defn- write-main-feed! [sources]
  (spit (fs/file output-dir "atom.xml")
        (atom-feed (reverse (blog-articles sources)))))

(defn- write-json-feed! [sources]
  (spit (fs/file output-dir "feed.json")
        (cheshire.core/generate-string
         (cske/transform-keys
          csk/->snake_case
          {:version "https://jsonfeed.org/version/1.1"
           :title "Jake McCrary's articles"
           :home-page-url "https://jakemccrary.com/"
           :feed-url "https://jakemccrary.com/feed.json"
           :favicon "https://jakemccrary.com/favicon.png"
           :author {:name "Jake McCrary"}
           :items (vec (for [article (take 20 (reverse (blog-articles sources)))
                             :let [output-file (:output-file article)
                                   {:keys [title local-date published]} (:metadata article)]
                             :when published
                             :let [link (str blog-root (str output-file))]]
                         (cond-> {:id link
                                  :url link
                                  :content-text (:html article)
                                  :title title
                                  :author {:name "Jake McCrary"}
                                  :date-published (rfc-3339 local-date)}
                           (:description article)
                           (assoc :summary (:description article)))))}))))



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
  (let [articles (reverse (blog-articles sources))]
    (doseq [category (categories articles)
            :let [related (filterv #(contains? (-> % :metadata :categories) category)
                                   articles)]
            :when (seq related)]
      (write-category-page! category related))))

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
                                  (cons (->yyyy-MM-dd (:start-date metadata))
                                        (when (:end-date metadata)
                                          ["to" (->yyyy-MM-dd (:end-date metadata))])))]]
      (when-not (clojure.string/blank? (:description metadata))
        [:p.post-description (:description metadata)])])])

(defn- write-adventures! [sources]
  (let [adventures (->> sources
                        (filterv adventure?)
                        (sort-by (comp :start-date :metadata))
                        reverse)]
    (write-html! (fs/file output-dir "adventures" "index.html")
                 {:body (hiccup/html
                         (adventure-list adventures))})))


#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn render [{:keys [preview] :as args}]
  (println "Rendering" args)
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
      (write-main-feed! sources)
      (write-json-feed! sources)
      (write-sitemap! sources))))

(defn extract-languages [source]
  (set (map second (re-seq #"language-(\w+)" (:html source)))))

(defn assert-code-highlighting! [sources]
  (let [supported-languages #{"clojure"}
        required-languages (into #{}
                                 (mapcat extract-languages)
                                 sources)]
    (when-let [missing (seq (clojure.set/difference required-languages
                                                    supported-languages))]
      (with-out-str
        (println "Missing" (clojure.string/join ", " (sort missing)) "from highlight.js")
        (println "Go regenerate at URL with languages:"
                 (->> (clojure.set/union supported-languages required-languages)
                      sort
                      (clojure.string/join ", ")))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn test [& args]
  (binding [*preview* true]
    (let [error (assert-code-highlighting! (load-sources))]
      (when error
        (println error)
        (System/exit -1)))))

(comment
  (def sources (doall (load-sources)))
  (def articles (blog-articles sources))

  (def adventures (filterv adventure? sources))
  (->yyyy-MM-dd (:start-date (:metadata (first adventures))))

  (:metadata (last articles))
  (def r *1)

  (write-adventures! sources)
  (type (:start-date (:metadata (first adventures))))

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

