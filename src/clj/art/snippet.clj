(require '[thi.ng.geom.core :as g]
         '[thi.ng.math.core :as m]
         '[thi.ng.geom.vector :as v]
         '[thi.ng.geom.spatialtree :as t])

(defn update-tree [tree]
  (fn [agents]
   (reduce
    #(g/add-point % %2 %2)
    (g/clear! tree)
    agents)))

(def agents (take 50 (repeatedly #(-> (v/randvec3) m/abs (m/* 500)))))
(def updater (update-tree (t/octree 0 0 0 500 500 500)))

(def tree (updater agents))
(updater agents) ;;exception


(g/clear! tree)

(g/add-point tree (v/vec3 1 1 1) nil)
