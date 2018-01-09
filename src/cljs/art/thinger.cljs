(ns art.thinger
  (:require [cljs.spec.alpha :as s]
            [cljs.spec.gen.alpha :as gen]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as tgen]
            [clojure.test.check.properties :as prop :include-macros true]
            [thi.ng.geom.core :as g]
            [thi.ng.math.core :as m]
            [thi.ng.geom.vector :as v]
            [thi.ng.geom.physics.core :as p]
            [thi.ng.geom.spatialtree :as t]
            [thi.ng.typedarrays.core :as ta]
            [taoensso.timbre :refer [info]]))

(def min-size (* -1 m/*eps*))
(def max-size (+ m/*eps* min-size))


(s/def ::coord (s/double-in :min min-size :max (+ m/*eps* max-size)
                            :NaN? true :infinite? true))
(s/def ::point (s/cat :x ::coord
                      :y ::coord
                      :z ::coord))


(comment

;; TODO: add test check
  
  (s/conform ::point [10 10 10])

  (dotimes [i 100]
    (info i)
    (reduce
     #(do 
        (g/add-point % %2 %2))
     (t/octree [0 0 0] max-size)
     (gen/sample (s/gen ::point) 1000)))

  )



