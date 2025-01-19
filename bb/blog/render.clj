(ns blog.render
  (:require [babashka.fs :as fs]
            [clojure.string]
            [hiccup2.core :as hiccup]
            [markdown.core :as markdown]
            [selmer.parser])
  (:import (java.time LocalDateTime LocalDate ZoneId)
           (java.time.format DateTimeFormatter DateTimeParseException)))

(defn left-pad [s padding n]
  (if (< (count s) n)
    (recur (str padding s) padding n)
    s))

(defmacro dbg [& args]
  `(let [r# (do ~@args)]
     (println (str "dbg: " (quote ~@args) " => " r#))
     r#))

(def source-dir "posts")
(def output-dir "output")
(def template-dir "templates")

(def default-render-opts
  {:site {:title "Jake McCrary"
          :subscribe-email "https://jakemccrary.substack.com/welcome"
          :subscribe-rss "rss"
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
                           ;; TODO(probably not going to work with adventure formats)
                           [(clojure.string/replace-first name #"\d\d\d\d-\d\d-\d\d-" "")
                            "index.html"]))))
(comment
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
   (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")])

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

(defn post-process-metadata [source]
  (cond-> source
    (-> source :metadata :date)
    (assoc-in [:metadata :local-date]
              (let [d (-> source :metadata :date)
                    parsed (cond
                             (inst? d)
                             (LocalDateTime/ofInstant (.toInstant d) (ZoneId/of "America/Chicago"))
                             :else
                             (parse-datetime d))]
                (if (instance? LocalDate parsed)
                  parsed
                  (.toLocalDate parsed))))))

(defn markdown->source [file]
  (let [markdown (slurp (fs/file file))]
    (println "Processing markdown for file:" (str file))
    (-> markdown
        ;; pre-process-markdown
        (markdown/md-to-html-string-with-meta :reference-links? true
                                              :code-style
                                              (fn [lang]
                                                (format "class=\"lang-%s\"" lang)))
        post-process-metadata
        #_#_        :html
        post-process-markdown)))

(defn load-sources []
  (when (fs/exists? source-dir)
    (for [path (fs/glob source-dir "**.markdown")]
      (as-> (markdown->source path) source
        (assoc source :input-file (fs/file path))
        (assoc source :output-file (output-file source))
        (assoc source :template (load-template source))))))

(defn blog-article? [{:keys [input-file]}]
  (clojure.string/includes? (str input-file) "/blog/"))

(defn blog-articles [sources]
  (sort-by (comp :local-date :metadata)
           (filter blog-article? sources)))

(defn write-html! [output-file m]
  (fs/create-dirs (fs/parent output-file))
  (when (fs/exists? output-file)
    (throw (Exception. "Tried to write file but it already exists:" (str output-file))))
  (spit output-file
        (selmer.parser/render
         (or (:template m) (slurp (fs/file template-dir "default.html")))
         (merge default-render-opts m))))

(defn write-post! [source]
  (let [out-file (fs/file output-dir (:output-file source))]
    (fs/create-dirs (fs/parent out-file))
    (write-html! out-file
                 {:body (:html source)
                  :template (:template source)})))

(defn blog-url [path]
  (-> path
      (clojure.string/replace "index.html" "")
      (cond->> (not (clojure.string/starts-with? path "/")) (str "/"))))

(defn- article-list [articles]
  [:ul
   (for [article articles
         :let [metadata (:metadata article)]]
     [:li
      [:time (str (:local-date metadata))] " "
      [:a
       {:href (blog-url (:output-file article))}
       (:title metadata)]])])

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

(defn- copy-resources []
  (doseq [f (fs/glob (fs/file source-dir) "**/*.{png,gif,jpeg,jpg,svg}")
            :let [out (apply fs/file output-dir (rest (fs/components f)))]]
      (fs/create-dirs (fs/parent out))
      (fs/copy f out)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn render []
  (println "Rendering")
  (when-not (fs/exists? output-dir)
    (fs/create-dir output-dir))

  (let [sources (load-sources)]
    (copy-resources)
    (run! write-post! sources)
    (write-index! sources)
    (write-archive! sources)))

(comment
  (def sources (load-sources))
  (def articles (blog-articles sources))

  (:metadata (first articles))

  (->> (mapv (juxt #(get-in % [:metadata :local-date])
                   #(get-in % [:metadata :date]))
             articles)
       (sort-by first))

  (parse-datetime "Sat Jun 05 19:00:00 CDT 2010")
;; :description => preview on main page?

;;
  )
