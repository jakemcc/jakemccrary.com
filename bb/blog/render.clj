(ns blog.render
  (:require [babashka.fs :as fs]
            [markdown.core :as markdown]))

(def output-dir "output")

(defn posts []
  (when (fs/exists? "posts")
    (fs/glob "posts" "*.markdown")))

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
  (markdown->html file))

(defn write-post! [file]
  (let [post (load-post file)]
    (spit (fs/file output-dir (clojure.string/replace (fs/file-name file)
                                                      ".markdown"
                                                      ".html"))
          (:html post))))

(defn render [& args]
  (println "Rendering")
  (when-not (fs/exists? output-dir)
    (fs/create-dir output-dir))
  (doseq [post (posts)]
    (write-post! post)))
