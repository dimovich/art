(ns art.agents
  (:require [thi.ng.geom.core :as g]
            [thi.ng.math.core :as m]
            [thi.ng.geom.vector :as v]
            [thi.ng.geom.physics.core :as p]
            [thi.ng.geom.spatialtree :as t]
            [thi.ng.typedarrays.core :as ta]
            [taoensso.timbre :refer [info]]))


(defrecord Agent [pos vel acc])


(defn generate-agents [n [x y z]]
  (into
   []
   (take n (repeatedly
            (fn []
              (map->Agent
               {:pos (v/vec3 (rand-int x)
                             (rand-int y)
                             (rand-int z))
                
                :vel (->> #(rand-nth (range -4.5 4.5 0.5))
                          repeatedly
                          (take 3)
                          (apply v/vec3))
                
                :acc (v/vec3)}))))))


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


(defn move [{count :count {:keys [pos vel acc]} :data}
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




(def reverse-x (v/vec3 -1 1 1))
(def reverse-y (v/vec3 1 -1 1))
(def reverse-z (v/vec3 1 1 -1))


(defn bounce [{count :count {:keys [pos vel acc]} :data}
              {[w h d] :size}]
  (loop [idx 0]
    (when (< idx count)
      (let [[x y z] (get-vec3 pos idx)
            avel (get-vec3 vel idx)]
        (set-vec3 vel idx
                  (cond-> avel
                    (or
                     (< x 0)
                     (> x w)) (m/* reverse-x)

                    (or
                     (< y 0)
                     (> y h)) (m/* reverse-y)

                    (or
                     (< z 0)
                     (> z d)) (m/* reverse-z)))
        
        (recur (inc idx))))))



(defn get-positions [{{:keys [pos]} :data
                      count :count}]
  (loop [idx 0 xs '()]
    (if (< idx count)
      (recur (inc idx) (cons (get-vec3 pos idx) xs))
      xs)))


(defn update-tree [tree]
  (fn [{{:keys [pos acc vel]} :data
        count :count}]
    (t/set-children tree nil)
    (loop [idx 0]
      (when (< idx count)
        (g/add-point tree (get-vec3 pos idx) idx)
        (recur (inc idx))))))


(defn swarm-separate [{:keys [pos vel acc]} idx others r mod]
  (let [force (v/vec3)
        apos (get-vec3 pos idx)]
    (-> (loop [others others force (v/vec3)]
          (if-let [o (first others)]
            (let [opos (get-vec3 pos o)
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


(defn swarm-align [{:keys [pos vel acc]} idx others radius mod]
  (let [apos (get-vec3 pos idx)
        avel (get-vec3 vel idx)]
    (-> (loop [others others force (v/vec3)]
          (if-let [o (first others)]
            (let [opos (get-vec3 pos o)
                  ovel (get-vec3 vel o)
                  dist (g/dist apos opos)]
              (recur (next others)
                     (if (zero? dist)
                       force
                       (m/+ force (m/* ovel (/ radius dist))))))
            force))
        (m/div (dec (count others)))
        m/normalize
        (m/* mod))))



(defn swarm-cohere [{:keys [pos vel acc]} idx others radius mod]
  (let [apos (get-vec3 pos idx)
        force (-> (loop [others others force (v/vec3)]
                    (if-let [o (first others)]
                      (recur (next others)
                             (if (= idx o)
                               force
                               (m/+ force (get-vec3 pos o))))
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
  (fn [{agent-count :count
        {:keys [pos acc] :as data} :data}
       {:keys [radius cohesion
               separation alignment
               view-angle max-vel size]}]
    (loop [idx 0]
      (when (< idx agent-count)
        (let [apos (get-vec3 pos idx)
              others (t/select-with-sphere tree apos radius)]
          (when (<= 2 (count others))
            (->> (reduce
                  m/+
                  (v/vec3)
                  [(swarm-separate data idx others radius separation)
                   (swarm-align data idx others radius alignment)
                   (swarm-cohere data idx others radius cohesion)])
                 (set-vec3 acc idx))))
        (recur (inc idx))))))
