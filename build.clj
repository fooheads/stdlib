(ns build
  "from https://kozieiev.com/blog/packaging-clojure-into-jar-uberjar-with-tools-build/"
  (:require
    [clojure.tools.build.api :as b]
    [deps-deploy.deps-deploy :as dd]))


(def build-folder "target")
(def jar-content (str build-folder "/classes"))


(def lib-name 'com.fooheads/stdlib)
(def version "0.1.12")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file-name (format "%s/%s-%s.jar" build-folder (name lib-name) version))


(defn clean
  [_]
  (b/delete {:path build-folder})
  (println (format "Build folder \"%s\" removed" build-folder)))


(defn jar
  [_]
  (clean nil)

  (b/copy-dir {:src-dirs   ["src" "resources"]
               :target-dir jar-content})

  (b/write-pom {:class-dir jar-content
                :lib       lib-name
                :version   version
                :basis     basis
                :src-dirs  ["src"]})

  (b/jar {:class-dir jar-content
          :jar-file  jar-file-name})
  (println (format "Jar file created: \"%s\"" jar-file-name)))


(defn deploy
  [_]
  (jar nil)

  (dd/deploy {:installer :remote
              :artifact jar-file-name
              :pom-file (b/pom-path {:lib lib-name
                                     :class-dir jar-content})}))

