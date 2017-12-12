(ns art.dynamic
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as qm]
            [amalloy.ring-buffer :as r]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.vector :as v]
            [thi.ng.math.core :as m]
            [thi.ng.geom.spatialtree :as t]
            [thi.ng.typedarrays.core :as ta]
            [art.agents :as a]
            [taoensso.timbre :refer [info]]))


(def config {:agent-count 70
             :color [200 200 0]
             :window-size [768 600]
             :size [800 600 500]
             :pos [0 0 -600]
             :radius 80
             :cohesion 0.38
             :separation 0.4
             :alignment 0.4
             :max-vel 6
             :trail-size 120})



(defonce state (atom {:active false}))


(defn setup []
  (let [tree (apply t/octree 0 0 0 (:size config))
        agents (a/generate-agents config)
        trail (a/->Trail (array) (:trail-size config) 0)]
    (q/frame-rate 60)
    (q/stroke-weight 0.5)
    (q/stroke 180)
    (q/no-fill)
    (q/background 0)
    (swap! state merge {:tick 0
                        :tree-updater (a/update-tree tree)
                        ;;:trail trail
                        :swarm-fn (a/swarm tree)
                        :trail (r/ring-buffer (:trail-size config))
                        :agents agents})))



(defn draw-box [[x y z]]
  ;;(q/no-fill)
  (q/stroke-weight 0.5)
  ;;(q/stroke 33)
  (q/with-translation [(/ x 2)  (/ y 2)  (/ z 2)]
    (q/box x y z)))


(defn draw-agents [agents]
  ;;(q/no-stroke)
  (q/stroke-weight 3)
  (q/begin-shape :points)

  (let [size (count agents)]
    (loop [idx 0]
      (when (< idx size)
        (let [a (aget agents idx)
              pos (.-pos a)
              color (m/normalize pos 255)]
          ;;(apply q/fill color)
          (q/stroke (get color 0)
                    (get color 1)
                    (get color 2))
          (q/vertex (get pos 0)
                    (get pos 1)
                    (get pos 2))
          #_(q/with-translation pos
              (q/box 10)))
        (recur (inc idx)))))
  (q/end-shape))


(defn draw-trail [trail]
  (q/stroke-weight 3)
  (q/begin-shape :points)
  (loop [trail trail]
    (when-let [p (first trail)]
      (apply q/stroke (m/normalize p 255))
      (apply q/vertex p)
      (recur (next trail))))
  (q/end-shape))


(defn update-state! [state]
  (let [{:keys [tree-updater agents swarm-fn]} @state]
    (swap! state update :trail into (a/get-positions agents))
    (tree-updater agents)
    (swarm-fn agents config)
    (a/move agents config)
    (a/bounce agents config)))


(defn draw []
  (update-state! state)
  
  (q/with-translation (:pos config)
    (q/background 0)
    ;;(draw-box (:size config))
    ;;(draw-agents (:agents @state))
    (draw-trail (:trail @state))))




(defn ^:export create []
  (q/with-sketch (q/get-sketch-by-id "art")
    (if (:active @state)
      (q/exit)
      (swap! state update :active not)))
  
  (q/defsketch art
    :renderer :p3d
    :setup setup
    :draw draw
    :size (:window-size config)))


(defn ^:export toggle []
  (q/with-sketch (q/get-sketch-by-id "art")
    (if (:active @state)
      (q/no-loop)
      (q/start-loop)))
  
  (swap! state update :active not))
