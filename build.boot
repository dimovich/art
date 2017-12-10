(set-env!
 :source-paths #{"src/clj" "src/cljs"}
 :resource-paths #{"resources"}
 :dependencies '[[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]

                 [adzerk/boot-cljs-repl     "0.3.3"  :scope "test"]
                 [adzerk/boot-cljs          "2.1.4"  :scope "test"]
                 [adzerk/boot-reload        "0.5.2"  :scope "test"]
                 [com.cemerick/piggieback   "0.2.2"  :scope "test"]
                 [weasel                    "0.7.0"  :scope "test"]
                 [pandeiro/boot-http "0.8.3"]

                 [org.clojure/tools.nrepl   "0.2.13"]
                 [cider/cider-nrepl         "0.15.1"]

                 [amalloy/ring-buffer "1.2.1"]
                 
                 [quil "2.6.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [thi.ng/geom "0.0.1178-SNAPSHOT"]
                 [thi.ng/math "0.2.1"]
                 [org.jogamp.gluegen/gluegen-rt "2.3.2" :classifier "natives-linux-amd64"]
                 [org.jogamp.jogl/jogl-all "2.3.2" :classifier "natives-linux-amd64"]])



(require
 '[pandeiro.boot-http    :refer [serve]]
 '[adzerk.boot-cljs      :refer [cljs]]
 '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
 '[adzerk.boot-reload    :refer [reload]])


(task-options! cljs  {:compiler-options
                      {:output-to  "js/main.js"
                       :output-dir "js/out"
                       :asset-path "js/out"
                       :parallel-build true
                       :main 'art.core
                       ;;:pseudo-names true
                       }})




(deftask dev
  []
  (task-options! cljs      {:optimizations :none
                            :source-map    true}
                 cljs-repl {:nrepl-opts {:port 3311}}
                 ;;reload {:on-jsload 'art.dynamic/run}
                 )
  (comp
   (cider)
   (serve :dir "target"
          :reload true
          :httpkit true
          :port 5000)
   (watch)
   (reload)
   (cljs-repl)
   (cljs)
   #_(repl :server true
           :port 3311)
   (target :dir #{"target"})))


(deftask prod
  []
  (task-options! cljs  {:optimizations :advanced})
  (comp
   (cljs)
   ;;(aot :namespace #{'art.core})
   ;;(uber)
   ;;(jar :file "art.jar" :main 'art.core)
   ;;(sift :include #{#"art.jar" #"js" #"index.html"})
   (target :dir #{"release"})))