(ns art.agents
  (:require [thi.ng.geom.core :as g]
            [thi.ng.math.core :as m]
            [thi.ng.geom.vector :as v]
            [thi.ng.geom.physics.core :as p]
            [thi.ng.geom.spatialtree :as t]
            [thi.ng.typedarrays.core :as ta]
            [taoensso.timbre :refer [info]]))


(defrecord Agent [pos vel acc])
(def POS 0)
(def VEL 1)
(def ACC 2)

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


(defn agents->array [agents]
  (let [agentsize 9
        array (ta/float32 (* agentsize (count agents)))
        agents (mapcat (fn [a]
                         (mapcat #(% a) [:pos :vel :acc]))
                       agents)]
    (doall
     (map-indexed (fn [i v]
                    (aset array i v))
                  agents))
    array))



(defn get-agent [arr idx prop]
  (let [idx (+ (* prop 3) (* idx 9))]
    (v/vec3 (aget arr idx)
            (aget arr (+ idx 1))
            (aget arr (+ idx 2)))))


(defn set-agent [arr idx prop v]
  (let [idx (+ (* prop 3) (* idx 9))
        [x y z] v]
    (aset arr idx x)
    (aset arr (+ idx 1) y)
    (aset arr (+ idx 2) z)))


(defn move [agents & [{:keys [max-vel
                              agent-count]}]]
  ;;(info agents)

  (loop [idx 0]
    (when (< idx agent-count)
      (let [pos (get-agent agents idx POS)
            vel (get-agent agents idx VEL)
            acc (get-agent agents idx ACC)]
        (let [force (m/limit (m/+ vel (m/normalize (v/randvec3) max-vel)) max-vel)]
          (set-agent agents idx POS (m/+ pos force))))
      (recur (inc idx)))))




(def reverse-x (v/vec3 -1 1 1))
(def reverse-y (v/vec3 1 -1 1))
(def reverse-z (v/vec3 1 1 -1))


(defn bounce [agents {[w h d] :size
                      :keys [agent-count]}]
  (loop [idx 0]
    (when (< idx agent-count)
      (let [pos (get-agent agents idx POS)
            vel (get-agent agents idx VEL)
            [x y z] pos]
        (set-agent agents idx VEL
                   (cond-> vel
                     (or (> x w)
                         (< x 0)) (m/* reverse-x)
                     (or (> y h)
                         (< y 0)) (m/* reverse-y)
                     (or (> z d)
                         (< z 0)) (m/* reverse-z)))
        
        (recur (inc idx))))))



(defn get-positions [agents {:keys [agent-count]}]
  (loop [idx 0 xs []]
    (if (< idx agent-count)
      (recur (inc idx) (conj xs (get-agent agents idx POS)))
      xs)))


(defn update-tree [tree]
  (fn [agents]
    (t/set-children tree nil)
    (loop [agents agents]
      (when-let [a (first agents)]
        (g/add-point tree (:pos a) a)
        (recur (next agents))))))


(defn swarm-separate [{pos :pos} others r mod]
  (let [force (v/vec3)]
    (loop [others others]
      (when-let [o (first others)]
        (let [dist (g/dist pos (:pos o))]
          (when-not (zero? dist)
            (m/+! force
                  (m/* (m/- pos (:pos o))
                       (/ (float r) dist))))
          (recur (next others)))))
    (m/* (m/normalize force)
         mod)))


(defn swarm-align [{pos :pos} others radius mod]
  (let [force (v/vec3)]
    (loop [others others]
      (when-let [{:keys [vel] :as o} (first others)]
        (let [dist (g/dist pos (:pos o))]
          (when-not (zero? dist)
            (m/+! force (m/* vel (/ (float radius) dist))))
          (recur (next others)))))
    (m/* (m/normalize force)
         mod)))


(defn swarm-cohere [{pos :pos} others radius mod]
  (let [force (v/vec3)]

    (loop [others others]
      (when-let [o (first others)]
        (m/+! force (:pos o))
        (recur (next others))))
    
    (m/div! force (dec (count others)))
    (-> (m/* (m/- force pos)
             (/ radius (g/dist pos force)))
        m/normalize
        (m/* mod))))



(defn swarm [tree]
  (fn [agents {:keys [radius cohesion
                      separation alignment
                      view-angle max-vel size]}]
    (loop [agents agents]
      (when-let [{:keys [acc pos] :as a} (first agents)]
        (let [others (t/select-with-sphere tree pos radius)]
          (when (< 1 (count others))
            (m/+! (:acc a) (m/normalize (v/randvec3) max-vel)
                  #_(-> (m/+ 
                         ;;(swarm-separate a others radius separation) 
                         ;;(swarm-align a others radius alignment)
                         ;;(swarm-cohere a others radius cohesion)
                         )
                        (m/normalize max-vel)))))
        (recur (next agents))))))
