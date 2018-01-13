(ns art.agents
  (:require [thi.ng.geom.core :as g]
            [thi.ng.math.core :as m]
            [thi.ng.geom.vector :as v]
            [thi.ng.geom.spatialtree :as t]
            [taoensso.timbre :refer [info]]))


(defrecord Agent [pos vel acc dist])


(defn generate-agents [{:keys [size max-vel agent-count]}]
  (let [xs (array)]
    (loop [idx 0]
      (when (< idx agent-count)
        (->> {:pos (apply v/vec3 (map rand-int size))
              :vel (v/randvec3 max-vel)
              :acc (v/vec3 0)
              :dist nil}
             map->Agent
             (aset xs idx))
        (recur (inc idx))))
    xs))


(defn move [agents {:keys [max-vel]}]
  (let [size (count agents)]
    (loop [idx 0]
      (when (< idx size)
        (let [a (aget agents idx)
              acc (.-acc a)
              pos (.-pos a)
              vel (.-vel a)
              f (m/limit (m/+ vel acc) max-vel)]
          (m/+! pos f)
          (m/*! vel 0)
          (m/+! vel f)
          (m/*! acc 0))
        (recur (inc idx))))))



(def reverse-x (v/vec3 -1 1 1))
(def reverse-y (v/vec3 1 -1 1))
(def reverse-z (v/vec3 1 1 -1))


(defn bounce [agents {[w h d] :size}]
  (let [size (count agents)]
    (loop [idx 0]
      (when (< idx size)
        (let [a (aget agents idx)
              [x y z] (.-pos a)
              vel     (.-vel a)]
          (cond-> vel
            (or
             (< x 0)
             (> x w)) (m/*! reverse-x)

            (or
             (< y 0)
             (> y h)) (m/*! reverse-y)

            (or
             (< z 0)
             (> z d)) (m/*! reverse-z))
        
          (recur (inc idx)))))))



(defn update-trail [trail agents]
  (if-let [a (first agents)]
    (recur (into trail (.-pos a))
           (next agents))
    trail))



(defn create-tree [agents {[w h d] :size}]
  (let [size (count agents)]
    (loop [idx 0
           tree (t/octree (* -1 w) (* -1 h) (* -1 d)
                          (* w 2) (* h 2) (* d 2))]
      (if (< idx size)
        (let [p (.-pos (aget agents idx))]
          (recur (inc idx)
                 (g/add-point tree p idx)))
        tree))))



(defn update-tree [tree]
  (fn [agents]
    (t/set-children tree nil)
    (let [size (count agents)]
      (loop [idx 0]
        (when (< idx size)
          (g/add-point tree (.-pos (aget agents idx)) idx)
          (recur (inc idx)))))))



(defn swarm-separate [agents a r mod]
  (let [apos (.-pos a)
        size (count agents)]
    (-> (loop [idx 0 force (v/vec3)]
          (if (< idx size)
            (let [other (get agents idx)
                  dist (.-dist other)
                  opos (.-pos other)]
              (recur (inc idx)
                     (if-not dist
                       force
                       (m/+ force
                            (m/* (m/- apos opos)
                                 (/ r dist))))))
            force))
        m/normalize
        (m/* mod))))



(defn swarm-align [agents a r mod]
  (let [apos (.-pos a)
        avel (.-vel a)
        size (count agents)]
    (-> (loop [idx 0 force (v/vec3) near 0]
          (if (< idx size)
            (let [other (get agents idx)
                  dist (.-dist other)
                  opos (.-pos other)
                  ovel (.-vel other)]
              (if-not dist
                (recur (inc idx) force near)
                (recur (inc idx) (m/+ force (m/* ovel (/ r dist))) (inc near))))
            (if (zero? near)
              force
              (m/div force near))))
        m/normalize
        (m/* mod))))



(defn swarm-cohere [agents a r mod]
  (let [apos (.-pos a)
        size (count agents)
        force (loop [idx 0 force (v/vec3) num 0]
                (if (< idx size)
                  (let [other (get agents idx)]
                    (if-not (.-dist other)
                      (recur (inc idx) force num)
                      (recur (inc idx) (m/+ force (.-pos other)) (inc num))))
                  (if (zero? num)
                    apos
                    (m/div force num))))
        
        dist (g/dist apos force)]
    
    (if (zero? dist)
      (v/vec3)
      (-> (m/* (m/- force apos)
               (/ r dist))
          m/normalize
          (m/* mod)))))



(defn distance [agents a others]
  (let [size (count others)
        pos (.-pos a)]
    (loop [idx 0 dists []]
      (if (< idx size)
        (let [o (get others idx)]
          (recur (inc idx)
                 (conj dists (g/dist pos (.-pos (aget agents o)))))) 
        dists))))



(defn distance2! [agents a r]
  (let [size (count agents)
        pos (.-pos a)]
    (loop [idx 0 num 0]
      (if (< idx size)
        (let [other (get agents idx)
              pos2 (.-pos other)
              d (g/dist pos pos2)]
          (if (or (zero? d) (> d r))
            (do
              (set! (.-dist other) nil)
              (recur (inc idx) num))
            (do
              (set! (.-dist other) d)
              (recur (inc idx) (inc num)))))
        num))))



(defn swarm
  [agents {:keys [radius cohesion separation
                  alignment size]}]
  
  (let [size (count agents)]
    (loop [idx 0]
      (when (< idx size)
        (let [a   (aget agents idx)
              acc (.-acc a)
              num (distance2! agents a radius)]
          (if (zero? num)
            (m/+! acc (v/randvec3))
            (m/+! acc (m/+ (swarm-separate agents a radius separation)
                           (swarm-align agents a radius alignment)
                           (swarm-cohere agents a radius cohesion)))))
        (recur (inc idx))))))
