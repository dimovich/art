(ns art.agents
  (:require [thi.ng.geom.core :as g]
            [thi.ng.math.core :as m]
            [thi.ng.geom.vector :as v]
            [thi.ng.geom.physics.core :as p]
            [thi.ng.geom.spatialtree :as t]
            [thi.ng.typedarrays.core :as ta]
            [taoensso.timbre :refer [info]]))


(defrecord Agent [pos vel acc])


(defn generate-agents [{:keys [size max-vel agent-count]}]
  (let [xs (array)]
    (loop [idx 0]
      (when (< idx agent-count)
        (->> {:pos (apply v/vec3 (map rand-int size))
              :vel (v/randvec3 max-vel)
              :acc (v/vec3 0)}
             map->Agent
             (aset xs idx ))
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
    (loop [idx 0 tree (t/octree (* -1 w) (* -1 h) (* -1 d) (* w 2) (* h 2) (* d 2))]
      (if (< idx size)
        (recur (inc idx)
               (g/add-point tree (.-pos (aget agents idx)) idx))
        tree))))



(defn update-tree [tree]
  (fn [agents]
    (t/set-children tree nil)
    (let [size (count agents)]
      (loop [idx 0]
        (when (< idx size)
          (g/add-point tree (.-pos (aget agents idx)) idx)
          (recur (inc idx)))))))



(defn swarm-separate [agents dists a others r mod]
  (let [apos (.-pos a)
        size (count others)]
    (-> (loop [idx 0 force (v/vec3)]
          (if (< idx size)
            (let [o (get others idx)
                  dist (get dists idx)
                  opos (.-pos (aget agents o))]
              (recur (inc idx)
                     (if (zero? dist)
                       force
                       (m/+ force
                            (m/* (m/- apos opos)
                                 (/ r dist))))))
            force))
        m/normalize
        (m/* mod))))



(defn swarm-align [agents dists a others radius mod]
  (let [apos (.-pos a)
        avel (.-vel a)
        size (count others)]
    (-> (loop [idx 0 force (v/vec3)]
          (if (< idx size)
            (let [o (get others idx)
                  dist (get dists idx)
                  opos (.-pos (aget agents o))
                  ovel (.-vel (aget agents o))]
              (recur (inc idx)
                     (if (zero? dist)
                       force
                       (m/+ force (m/* ovel (/ radius dist))))))
            force))
        (m/div (dec size))
        m/normalize
        (m/* mod))))



(defn swarm-cohere [agents dists a others radius mod]
  (let [apos (.-pos a)
        force (-> (loop [others others force (v/vec3)]
                    (if-let [idx (first others)]
                      (let [o (aget agents idx)]
                        (recur (next others)
                               (if (= a o)
                                 force
                                 (m/+ force (.-pos o)))))
                      force))
                  (m/div (dec (count others))))
        dist (g/dist apos force)]
    
    (if (zero? dist)
      (v/vec3)
      (-> (m/* (m/- force apos)
               (/ radius dist))
          m/normalize
          (m/* mod))) ))



(defn distance [agents a others]
  (let [size (count others)
        pos (.-pos a)]
    (loop [idx 0 dists []]
      (if (< idx size)
        (let [o (get others idx)]
          (recur (inc idx)
                 (conj dists (g/dist pos (.-pos (aget agents o)))))) 
        dists))))



(defn swarm [tree {:keys [agent-count]}]
  (fn [agents
       {:keys [radius cohesion
               separation alignment
               view-angle max-vel size] :as config}]

    (let [size (count agents)
          tree (create-tree agents config)]
      (loop [idx 0]
        (when (< idx size)
          (let [a (aget agents idx)
                acc (.-acc a)
                pos (.-pos a)
                others (t/select-with-sphere tree pos radius)
                dists (distance agents a others)]
            (if (<= 2 (count others))
              (m/+! acc (m/+ (swarm-separate agents dists a others radius separation)
                             (swarm-align agents dists a others radius alignment)
                             (swarm-cohere agents dists a others radius cohesion)))
              (m/+! acc (v/randvec3))))
          (recur (inc idx)))))))
