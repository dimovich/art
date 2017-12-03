(ns art.agents
  (:require [thi.ng.geom.core :as g]
            [thi.ng.geom.core.vector :as v]
            [thi.ng.geom.spatialtree :as t]
            [taoensso.timbre :refer [info]]))


(defn generate-agents [n [x y z]]
  (into
   []
   (take n (repeatedly
            (fn []
              {:pos (v/vec3 (rand-int x)
                            (rand-int y)
                            (rand-int z))

               :acc (v/vec3)
               
               :vel (->> #(rand-nth (range -1.5 2 0.5))
                         repeatedly
                         (take 3)
                         (apply v/vec3))})))))



(defn move [agents & [{:keys [max-vel]}]]
  (mapv
   (fn [agent]
     (let [vel (:vel agent)
           acc (:acc agent)
           vel (g/limit (g/+ vel acc)
                        max-vel)]
       (-> agent
           (update :pos g/+ vel)
           (assoc :vel vel
                  :acc (v/vec3)))))
   agents))



(def reverse-x (v/vec3 -1 1 1))
(def reverse-y (v/vec3 1 -1 1))
(def reverse-z (v/vec3 1 1 -1))


(defn bounce [agents {[w h d] :size}]
  (mapv
   (fn [{[x y z :as pos] :pos :as agent}]
     (update agent :vel
             #(cond-> %
                (or (> x w)
                    (< x 0)) (g/* reverse-x)
                (or (> y h)
                    (< y 0)) (g/* reverse-y)
                (or (> z d)
                    (< z 0)) (g/* reverse-z))))
   agents))


(defn update-tree [tree agents]
  (reduce
   #(g/add-point % (:pos %2) %2)
   (g/clear! tree)
   agents))


(defn swarm-separate [a others radius]
  (let [force (reduce
               (fn [v o]
                 (g/+ v
                      (g/* (g/- (:pos a)
                                (:pos o))
                           (/ radius (:dist o)))))
               (v/vec3)
               others)]
    (when (< 0 (g/mag-squared force))
      (g/normalize force))))


(defn swarm-align [a others radius]
  (let [force (reduce
               (fn [v o]
                 (g/+ v
                      (g/* (:vel o)
                           (/ radius (:dist o)))))
               (v/vec3)
               others)]
    (when (< 0 (g/mag-squared force))
      (g/normalize force))))


(defn swarm-cohere [a others radius]
  (let [cohesion (reduce
                  (fn [v o]
                    (g/+ v (:pos o)))
                  (v/vec3)
                  others)
        scaled (g/* cohesion (/ 1.0 (count others)))
        dist (g/dist (:pos a) scaled)
        force (g/* (g/- scaled (:pos a))
                   (/ radius dist))]
    (when (< 0 (g/mag-squared force))
      (g/normalize force))))



(defn distance [a others]
  (mapv
   (fn [o]
     (assoc o :dist (g/dist (:pos a) (:pos o))))
   others))


(defn swarm [agents {:keys [radius cohesion
                            separation alignment
                            view-angle max-vel size]}]

  (let [tree (apply t/octree 0 0 0 (map (partial * 2) size))]

    (doseq [a agents]
      (g/add-point tree (:pos a) a))
    
    (mapv
     (fn [a]
       (let [others (->> (t/select-with-sphere tree (:pos a) radius)
                         (remove #{a})
                         (distance a)
                         (remove #(zero? (:dist %))))]
         (if (empty? others)
           a
           (let [force (reduce
                        g/+
                        (v/vec3)
                        (remove
                         nil?
                         [(some-> (swarm-separate a others radius)
                                  (g/* separation)) 
                          (some-> (swarm-align a others radius)
                                  (g/* alignment))
                          (some-> (swarm-cohere a others radius)
                                  (g/* cohesion))]))
                 acc (g/+ (:acc a) force)]
             (assoc a :acc acc)))))
     agents)))
