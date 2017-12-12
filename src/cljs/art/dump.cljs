(ns art.dump
  (:require 
   [thi.ng.geom.core :as g]
   [thi.ng.geom.physics.core :as p]
   [thi.ng.math.core :as m]
   [thi.ng.geom.vector :as v]
   [thi.ng.geom.spatialtree :as t]
   [taoensso.timbre :refer [info]]
   [art.agents :as a]))

#_(
 (def config {:agent-count 100
              :color [200 200 0]
              :window-size [768 560]
              :size [1000 500 500]
              :radius 60
              :cohesion 0.27
              :separation 0.30
              :alignment 0.55
              :max-vel 6
              :view-angle 250})


 (def agents (a/generate-agents 50 [500 500 500]))

 (def ps (->> agents
              (map (fn [{:keys [pos vel]}]
                     (-> (p/particle pos)
                         (p/add-force vel))))))


 (defn update-accelerator
   [accel]
   (fn [physics _]
     (reduce
      #(g/add-point % (p/position %2) %2)
      (g/clear! accel)
      (:particles physics))))



 (defn attract!
   [p q rsq strength delta]
   (let [d (g/- p (p/position q))
         l (+ (g/mag-squared d) 1e-6)]
     (if (< l rsq)
       (p/add-force
        q (g/* d (/ (* (- 1.0 (/ l rsq)) (* strength delta))
                    (Math/sqrt l)))))))




 (defn swarm [p q r]
   (mapv
    (fn [a]
      (let [others (->> (t/select-with-sphere tree (:pos a) radius)
                        (remove #{a})
                        (distance a)
                        (remove #(zero? (:dist %))))]
        (if (empty? others)
          a
          (let [force (reduce
                       m/+
                       (v/vec3)
                       (remove
                        nil?
                        [(some-> (swarm-separate a others radius)
                                 (m/* separation)) 
                         (some-> (swarm-align a others radius)
                                 (m/* alignment))
                         (some-> (swarm-cohere a others radius)
                                 (m/* cohesion))]))
                acc (m/+ (:acc a) force)]
            (assoc a :acc acc)))))
    agents))



 (defn agent-swarm
   [tree {:keys [radius cohesion
                 separation alignment
                 view-angle max-vel size]}]
   (fn [p delta]
     (let [p' (p/position p)]
       (loop [neighbors (t/select-with-sphere tree p' radius)]
         (when-let [n (first neighbors)]
           (if-not (= p n) (swarm p n radius))
           (recur (next neighbors)))))))



 #_(phys/physics
    {:particles ps
     :behaviors {:g (phys/gravity (:gravity state))
                 :f (agent-swarm tree config)}
     :listeners {:iter (update-tree tree)}}))





#_(extend-type t/MutableOctreeNode
    g/IClear
    (clear!
      [_]
      (t/set-children _ nil)
      _))



#_(
   (defn agents->array [agents ks]
     (reduce
      (fn [m k]
        (assoc m k (ta/float32 (mapcat k agents))))
      {}
      ks))



   (defn get-vec3 [data idx]
     (v/vec3 (aget data (* 3 idx))
             (aget data (+ 1 (* 3 idx)))
             (aget data (+ 2 (* 3 idx)))))


   (defn set-vec3 [data idx [x y z]]
     (aset data (* 3 idx) x)
     (aset data (+ 1 (* 3 idx)) y)
     (aset data (+ 2 (* 3 idx)) z))

   )



#_(defn move [{count :count {:keys [pos vel acc]} :data}
              {:keys [max-vel]}]
    (loop [idx 0]
      (when (< idx count)
        (let [apos (get-vec3 pos idx)
              avel (get-vec3 vel idx)
              aacc (get-vec3 acc idx)]
          (let [force (m/limit (m/+ avel aacc) max-vel)]
            (set-vec3 pos idx (m/+ apos force))
            (set-vec3 vel idx force)
            (set-vec3 acc idx [0 0 0])))
        (recur (inc idx)))))
