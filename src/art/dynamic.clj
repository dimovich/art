(ns art.dynamic
  (:require [quil.core :as q]
            [thi.ng.geom :as tg]))


(defn generate-agents [n]
  (let [r (range -2 3)]
   (into [] (take n (partition 3 (repeatedly #(rand-nth r)))))))


(defn setup []
  (q/smooth)
  (q/frame-rate 120)
  (q/background 200)
  {:agents (generate-agents 50)})

(defn update [state]
  state)

(defn draw [state]
  (q/stroke (q/random 255) (q/random 255) (q/random 255))
  (q/stroke-weight (q/random 10))
  (q/fill (q/random 255) (q/random 255) (q/random 255))

  (let [diam (q/random 100)
        x    (q/random (q/width))
        y    (q/random (q/height))]
    (q/ellipse x y diam diam))
  state)
