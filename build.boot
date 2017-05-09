(set-env!
 :source-paths #{"src"}
 ;;:resource-paths #{"resource"}
 :dependencies '[[org.clojure/clojure "1.8.0" :scope "provided"]
                 [quil "2.6.0"]
                 [thi.ng/geom "0.0.908"]])

(require 'boot.repl)

(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.15.0-SNAPSHOT"]])

(swap! boot.repl/*default-middleware*
       conj 'cider.nrepl/cider-middleware)


(deftask dev
  []
  (comp
   (watch)
   (repl :server true)
   (target :dir #{"target"})))


(deftask build
  []
  (comp
   (aot :namespace #{'brun.core})
   (uber)
   (jar :file "art.jar" :main 'art.core)
   (sift :include #{#"art.jar"})
   (target :dir #{"target"})))
