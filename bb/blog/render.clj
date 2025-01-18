(ns blog.render
  (:require [babashka.fs :as fs]
            [markdown.core :as markdown]
            [selmer.parser]
            [hiccup2.core :as hiccup]))

(def source-dir "posts")
(def output-dir "output")
(def template-dir "templates")

(def default-render-opts
  {:site {:title "Jake McCrary"}})

(defn posts []
  (when (fs/exists? source-dir)
    (fs/glob source-dir "**.markdown")))

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

(defn load-post [file]
  (assoc (markdown->html file)
         :path file))

(defn load-template [post]
  (let [template "default" #_ (or (-> post :metadata :layout) "default")
        file (fs/file template-dir (str template ".html"))]
    (if (fs/exists? file)
      (slurp file)
      (throw (Exception. (str "template not found: " (str file)))))))

(defn output-file [post]
  (let [sub-dirs (->> (fs/components (:path post))
                      (rest)
                      (butlast))
        name (clojure.string/replace (fs/file-name (:path post))
                                     ".markdown"
                                     ".html")]
    (apply fs/file (concat sub-dirs [name]))))

(defn write-post! [file]
  (let [post (load-post file)
        template (load-template post)
        out-file (fs/file output-dir (output-file post))]
    (fs/create-dirs (fs/parent out-file))
    (spit out-file
          (selmer.parser/render template (assoc default-render-opts
                                                :body (:html post))))))

(defn blog-article? [{:keys [path]}]
  (clojure.string/includes? (str path) "/blog/"))

(defmacro dbg [& args]
  `(let [r# (do ~@args)]
     (println (str "dbg: " (quote ~@args) " => " r#))
     r#))

(defn write-index! [posts]
  (let [articles (dbg (filterv blog-article? posts))]
    (spit (fs/file output-dir "index.html")
          (selmer.parser/render
           (slurp (fs/file template-dir "default.html"))
           (assoc default-render-opts
                  :body (hiccup/html [:div 
                                      (for [article articles
                                            :let [out-file (output-file article)]]
                                        [:a {:href (str out-file)}
                                         (or (-> article :metadata :title) "A title")])])))))
  )

(defn render [& args]
  (println "Rendering")
  (when-not (fs/exists? output-dir)
    (fs/create-dir output-dir))
  (doseq [post (posts)]
    (write-post! post))
  (write-index! (mapv load-post (posts))))

(comment
  (render)
  
  (def a-post (first (posts)))
  (load-post a-post)
  (output-file (load-post a-post))

  (fs/components a-post)


  (load-template (load-post (first (posts))))

  (write-post! (first (posts)))



;;
)
