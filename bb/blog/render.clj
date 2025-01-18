(ns blog.render
  (:require [babashka.fs :as fs]
            [clojure.string]
            [hiccup2.core :as hiccup]
            [markdown.core :as markdown]
            [selmer.parser]))

(defmacro dbg [& args]
  `(let [r# (do ~@args)]
     (println (str "dbg: " (quote ~@args) " => " r#))
     r#))

(def source-dir "posts")
(def output-dir "output")
(def template-dir "templates")

(def default-render-opts
  {:site {:title "Jake McCrary"}})

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

(defn markdown->html [file]
  (let [markdown (slurp (fs/file file))]
    (println "Processing markdown for file:" (str file))
    (-> markdown
        ;; pre-process-markdown
        (markdown/md-to-html-string-with-meta :reference-links? true
                                              :code-style
                                              (fn [lang]
                                                (format "class=\"lang-%s\"" lang)))
        #_#_        :html
        post-process-markdown)))

(defn load-sources []
  (when (fs/exists? source-dir)
    (for [path (fs/glob source-dir "**.markdown")]
      (as-> (markdown->html path) source
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

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn render []
  (println "Rendering")
  (when-not (fs/exists? output-dir)
    (fs/create-dir output-dir))

  (let [sources (load-sources)]
    (run! write-post! sources)
    (write-index! sources)))
