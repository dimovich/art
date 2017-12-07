(ns art.snippet
  (:require [thi.ng.geom.core :as g]
            [thi.ng.math.core :as m]
            [thi.ng.geom.vector :as v]
            [thi.ng.geom.spatialtree :as t]))

(defn update-tree [tree]
  (fn [agents]
    (reduce
     #(g/add-point % %2 %2)
     (t/set-children tree nil)
     agents)))

(def agents (take 50 (repeatedly #(-> (v/randvec3) m/abs (m/* 500)))))
(def updater (update-tree (t/octree 0 0 0 500 500 500)))

tree
(def tree (updater agents))
#_(updater agents)
;;exception

(count (t/get-children (updater agents)))



;;(g/add-point tree (v/vec3 1 1 1) nil)

