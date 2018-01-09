(ns art.agents
  (:require [thi.ng.geom.core :as g]
            [thi.ng.math.core :as m]
            [thi.ng.geom.vector :as v]
            [thi.ng.geom.physics.core :as p]
            [thi.ng.geom.spatialtree :as t]
            [thi.ng.typedarrays.core :as ta]
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



(defn add-point*
  "Associates point with data in tree, recursively creates all required intermediate nodes."
  [root p d]
  (loop [node root, p p, d d]
    (info "adding" p)
    (if (t/get-children node)
      (recur (t/make-child-for-point node p d false) p d)
      (let [point (g/get-point node)]
        (if point
          (if-not (m/delta= point p m/*eps*)
            (let [data (g/get-point-data node)]
              (t/split-node node)
              (t/make-child-for-point node p d true)
              (recur node point data)))
          (t/set-point node p d)))))
  root)


(defn create-tree [agents {[w h d] :size}]
  (let [size (count agents)]
    (loop [idx 0
           tree (t/octree (* -1 w) (* -1 h) (* -1 d)
                          (* w 2) (* h 2) (* d 2))]
      (if (< idx size)
        (let [p (.-pos (aget agents idx))]
          (recur (inc idx)
                 (g/add-point tree p idx)
                 #_(if (and (pos? (get p 0))
                            (pos? (get p 1))
                            (pos? (get p 2)))
                     (add-point* tree p idx)
                     tree)))
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
  (let [apos (.-pos a)]
    (-> (loop [agents agents force (v/vec3)]
          (if-let [other (first agents)]
            (let [dist (.-dist other)
                  opos (.-pos other)]
              (recur (next agents)
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
        avel (.-vel a)]
    (-> (loop [agents agents force (v/vec3) size 0]
          (if-let [other (first agents)]
            (let [dist (.-dist other)
                  opos (.-pos other)
                  ovel (.-vel other)]
              (if-not dist
                (recur (next agents) force size)
                (recur (next agents) (m/+ force (m/* ovel (/ r dist))) (inc size))))
            (if (zero? size)
              force
              (m/div force size))))
        m/normalize
        (m/* mod))))



(defn swarm-cohere [agents a r mod]
  (let [apos (.-pos a)
        force (loop [agents agents force (v/vec3) size 0]
                (if-let [other (first agents)]
                  (if-not (.-dist other)
                    (recur (next agents) force size)
                    (recur (next agents) (m/+ force (.-pos other)) (inc size)))                      
                  (if (zero? size)
                    force
                    (m/div force size))))
        
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
    (loop [agents agents]
      (when-let [other (first agents)]
        (let [pos2 (.-pos other)
              d (g/dist pos pos2)]
          (set! (.-dist other)
                (when-not (or (zero? d)
                              (< r d))
                  d))
          (recur (next agents)))))))



(defn swarm [{:keys [radius cohesion
                     separation alignment
                     view-angle max-vel size] :as config}]
  (fn [agents]
    (let [size (count agents)]
      (loop [idx 0]
        (when (< idx size)
          (let [a (aget agents idx)]
            (distance2! agents a radius)
            (m/+! (.-acc a) (m/+ (swarm-separate agents a radius separation)
                                 (swarm-align agents a radius alignment)
                                 (swarm-cohere agents a radius cohesion))))
          (recur (inc idx)))))))
