(ns art.agents
  (:require [thi.ng.geom.core :as g]
            [thi.ng.math.core :as m]
            [thi.ng.geom.vector :as v]
            [thi.ng.geom.physics.core :as p]
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
           vel (m/limit (m/+ vel acc)
                        max-vel)]
       (-> agent
           (update :pos m/+ vel)
           (assoc :vel vel
                  :acc (v/vec3)))))
   agents))



(def reverse-x (v/vec3 -1 1 1))
(def reverse-y (v/vec3 1 -1 1))
(def reverse-z (v/vec3 1 1 -1))


(extend-type t/MutableOctreeNode
  g/IClear
  (clear!
    [_]
    (t/set-children _ nil)
    _))


(defn bounce [agents {[w h d] :size}]
  (mapv
   (fn [{[x y z :as pos] :pos :as agent}]
     (update agent :vel
             #(cond-> %
                (or (> x w)
                    (< x 0)) (m/* reverse-x)
                (or (> y h)
                    (< y 0)) (m/* reverse-y)
                (or (> z d)
                    (< z 0)) (m/* reverse-z))))
   agents))


(defn update-tree [tree]
  (fn [agents]
    (reduce
     #(g/add-point % (:pos %2) %2)
     (g/clear! tree)
     agents)))


(defn swarm-separate [p others r]
  (let [pos (:pos p)]
    (m/normalize
     (reduce
      #(m/+ % (m/* (m/- pos
                        (:pos %2))
                   (/ r (:dist %2))))
      (v/vec3)
      others))))


(defn swarm-align [a others radius]
  (m/normalize
   (reduce
    #(m/+
      % (m/*
         (:vel %2)
         (/ radius (:dist %2))))
    (v/vec3)
    others)))


(defn swarm-cohere [a others radius]
  (let [cohesion (reduce
                  #(m/+ % (:pos %2))
                  (v/vec3)
                  others)
        scaled (m/div cohesion (count others))
        dist (g/dist (:pos a) scaled)
        force (m/* (m/- scaled (:pos a))
                   (/ radius dist))]
    (m/normalize force)))



(defn distance [a others]
  (for [o others]
    (assoc o :dist (g/dist (:pos a) (:pos o)))))


(defn swarm [tree {:keys [radius cohesion
                          separation alignment
                          view-angle max-vel size]}]
  (fn [agents]
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
                        [(-> (swarm-separate a others radius)
                             (m/* separation)) 
                         (-> (swarm-align a others radius)
                             (m/* alignment))
                         (-> (swarm-cohere a others radius)
                             (m/* cohesion))])
                 acc (m/+ (:acc a) force)]
             (assoc a :acc acc)))))
     agents)))
