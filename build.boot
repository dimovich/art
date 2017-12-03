(set-env!
 :source-paths #{"src/clj"}
 :resource-paths #{"resources"}
 :dependencies '[[org.clojure/clojure "1.9.0-RC2"]
                 [quil "2.6.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [thi.ng/geom "0.0.908"]])


(deftask dev
  []
  (comp
   (cider)
   (watch)
   (repl :server true
         :port 3311)
   (target :dir #{"target"})))


(deftask build
  []
  (comp
   (aot :namespace #{'art.core})
   (uber)
   (jar :file "art.jar" :main 'art.core)
   (sift :include #{#"art.jar"})
   (target :dir #{"target"})))
