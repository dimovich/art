(ns art.agents
  (:require [thi.ng.geom.core :as g]
            [thi.ng.math.core :as m]
            [thi.ng.geom.vector :as v]
            [thi.ng.geom.physics.core :as p]
            [thi.ng.geom.spatialtree :as t]
            [thi.ng.typedarrays.core :as ta]
            [taoensso.timbre :refer [info]]))


(defrecord Agent [pos vel acc])
(defrecord Trail [arr size idx])



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
        (let [a (aget agents idx)]
          (aset a "vel"
                (m/limit (m/+ (.-vel a) (.-acc a)) max-vel))
          (m/+! (.-pos a) (.-vel a))
          (m/*! (.-acc a) 0))
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




(defn get-positions [agents]
  (map #(apply v/vec3 (:pos %)) agents)
  #_(reduce
     #(cons (.-pos %2) )
     '()
     agents)
  #_(let [size (count agents)
          arr (array)]
      (loop [idx 0]
        (when (< idx size)
          (->> (aget agents idx "pos")
               (apply v/vec3)
               (aset arr idx))
          (recur (inc idx))))
      arr))


(defn update-tree [tree]
  (fn [agents]
    (t/set-children tree nil)
    (dotimes [idx (count agents)]
      (g/add-point tree (aget agents idx "pos") idx))
    
    #_(let [size (count agents)]
        (loop [idx 0]
          (when (< idx size)
            (g/add-point tree (get-vec3 pos idx) idx)
            (recur (inc idx)))))))


(defn swarm-separate [agents idx others r mod]
  (let [apos (aget agents idx "pos")]
    (-> (loop [others others force (v/vec3)]
          (if-let [o (first others)]
            (let [opos (aget agents o "pos")
                  dist (g/dist apos opos)]
              (recur (next others)
                     (if (zero? dist)
                       force
                       (m/+ force
                            (m/* (m/- apos opos)
                                 (/ r dist))))))
            force))
        m/normalize
        (m/* mod))))


(defn swarm-align [agents idx others radius mod]
  (let [apos (aget agents idx "pos")
        avel (aget agents idx "vel")]
    (-> (loop [others others force (v/vec3)]
          (if-let [o (first others)]
            (let [opos (aget agents o "pos")
                  ovel (aget agents o "vel")
                  dist (g/dist apos opos)]
              (recur (next others)
                     (if (zero? dist)
                       force
                       (m/+ force (m/* ovel (/ radius dist))))))
            force))
        (m/div (dec (count others)))
        m/normalize
        (m/* mod))))



(defn swarm-cohere [agents idx others radius mod]
  (let [apos (aget agents idx "pos")
        force (-> (loop [others others force (v/vec3)]
                    (if-let [o (first others)]
                      (recur (next others)
                             (if (= idx o)
                               force
                               (m/+ force (aget agents o "pos"))))
                      force))
                  (m/div (dec (count others))))
        dist (g/dist apos force)]
    
    
    
    (if (zero? dist)
      (v/vec3)
      (-> (m/* (m/- force apos)
               (/ radius dist))
          m/normalize
          (m/* mod))) ))



(defn swarm [tree]
  (fn [agents
       {:keys [radius cohesion
               separation alignment
               view-angle max-vel size]}]
    (let [size (count agents)]
      (loop [idx 0]
        (when (< idx size)
          (let [others (t/select-with-sphere tree (aget agents idx "pos") radius)]
            (when (<= 2 (count others))
              (m/+! (aget agents idx "acc")
                    (m/+ (swarm-separate agents idx others radius separation)
                         (swarm-align agents idx others radius alignment)
                         (swarm-cohere agents idx others radius cohesion))) #_(aset agents
                    idx "acc")))
          (recur (inc idx)))))))
