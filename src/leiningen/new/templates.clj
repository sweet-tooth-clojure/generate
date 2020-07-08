;; copied from Leiningen
;; https://github.com/technomancy/leiningen/blob/stable/src/leiningen/new/templates.clj
;; licensed under the EPL 1.0
;; https://www.eclipse.org/legal/epl-v10.html
(ns leiningen.new.templates
  (:require [clojure.string :as string]))

(defn project-name
  "Returns project name from (possibly group-qualified) name:
  mygroup/myproj => myproj
  myproj         => myproj"
  [s]
  (last (string/split s #"/")))

(defn sanitize
  "Replace hyphens with underscores."
  [s]
  (string/replace s "-" "_"))

(defn multi-segment
  "Make a namespace multi-segmented by adding another segment if necessary.
  The additional segment defaults to \"core\"."
  ([s] (multi-segment s "core"))
  ([s final-segment]
     (if (.contains s ".")
       s
       (format "%s.%s" s final-segment))))

(defn name-to-path
  "Constructs directory structure from fully qualified artifact name:
  \"foo-bar.baz\" becomes \"foo_bar/baz\"
  and so on. Uses platform-specific file separators."
  [s]
  (-> s sanitize (string/replace "." java.io.File/separator)))

(defn sanitize-ns
  "Returns project namespace name from (possibly group-qualified) project name:
  mygroup/myproj  => mygroup.myproj
  myproj          => myproj
  mygroup/my_proj => mygroup.my-proj"
  [s]
  (-> s
      (string/replace "/" ".")
      (string/replace "_" "-")))

(defn group-name
  "Returns group name from (a possibly unqualified) name:
  my.long.group/myproj => my.long.group
  mygroup/myproj       => mygroup
  myproj               => nil"
  [s]
  (let [grpseq (butlast (string/split (sanitize-ns s) #"\."))]
    (if (seq grpseq)
      (->> grpseq (interpose ".") (apply str)))))
