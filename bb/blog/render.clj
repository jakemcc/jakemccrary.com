(ns blog.render
  (:require [babashka.fs :as fs]
            [clojure.string]
            [hiccup2.core :as hiccup]
            [markdown.core :as markdown]
            [selmer.parser])
  (:import (java.time LocalDateTime LocalDate ZoneId)
           (java.time.format DateTimeFormatter DateTimeParseException)))

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

(defn output-file [post]
  (let [sub-dirs (->> (fs/components (:input-file post))
                      (rest)
                      (butlast))
        name (clojure.string/replace (fs/file-name (:input-file post))
                                     ".markdown"
                                     ".html")]
    (apply fs/file (concat sub-dirs [name]))))

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

(defn write-post! [source]
  (let [out-file (fs/file output-dir (:output-file source))]
    (fs/create-dirs (fs/parent out-file))
    (spit out-file
          (selmer.parser/render (:template source)
                                (assoc default-render-opts
                                       :body (:html source))))))

(defn blog-article? [{:keys [input-file]}]
  (clojure.string/includes? (str input-file) "/blog/"))

(defn blog-articles [sources]
  (sort-by (comp :local-date :metadata)
           (filter blog-article? sources)))

(defn write-html! [output-file m]
  (fs/create-dirs (fs/parent output-file))
  (spit output-file
        (selmer.parser/render
         (slurp (fs/file template-dir "default.html"))
         (merge default-render-opts m))))

(defn write-index! [sources]
  (let [articles (filterv blog-article? sources)]
    (spit (fs/file output-dir "index.html")
          (selmer.parser/render
           (slurp (fs/file template-dir "default.html"))
           (assoc default-render-opts
                  :body (hiccup/html
                         [:div 
                          (for [article articles
                                :let [out-file (:output-file article)]]
                            [:a {:href (str out-file)}
                             (or (-> article :metadata :title) "A title")])]))))))

(defn write-archive! [sources]
  (let [articles (reverse (blog-articles sources))]
    (write-html! (fs/file output-dir "blog" "archives" "index.html")
                 {:body (hiccup/html
                         [:div {:id "blog-archives"}
                          [:ul
                           (for [article articles
                                 :let [metadata (:metadata article)]]
                             [:li
                              [:time (str (:local-date metadata))] " "
                              [:a
                               {:href (str "/" (:output-file article))}
                               (:title metadata)]])]])})))


#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn render []
  (println "Rendering")
  (when-not (fs/exists? output-dir)
    (fs/create-dir output-dir))

  (let [sources (load-sources)]
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
