(ns art.dynamic
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as qm]
            [amalloy.ring-buffer :as r]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.vector :as v]
            [thi.ng.math.core :as m]
            [thi.ng.geom.spatialtree :as t]
            [art.agents :as a]
            [taoensso.timbre :refer [info]]))


(def config {:agent-count 300
             :color [200 200 0]
             :window-size [768 600]
             :size [800 500 500]
             :pos [-50 50 -700]
             :radius 60
             :cohesion 0.3
             :separation 0.3
             :alignment 0.8
             :max-vel 6
             :trail-size 800})



(defonce state (atom {:active false}))


(defn setup []
  (let [tree (apply t/octree 0 0 0 (:size config))
        agents (a/agents->array (apply a/generate-agents
                                       (mapv config [:agent-count :size])))]
    (q/frame-rate 60)
    (q/stroke-weight 0.5)
    (q/stroke 180)
    (q/no-fill)
    (q/background 0)
    (swap! state merge {:tick 0
                        :tree-updater (a/update-tree tree)
                        :swarm-fn (a/swarm tree)
                        :trail (r/ring-buffer (:trail-size config))
                        :agents agents})))



(defn draw-box [[x y z]]
  (q/no-fill)
  (q/stroke-weight 0.5)
  (q/stroke 33)
  (q/with-translation [(/ x 2)  (/ y 2)  (/ z 2)]
    (q/box x y z)))


(defn draw-agents [agents]
  (q/no-stroke)
  (doseq [{:keys [pos vel acc]} agents]
    (apply q/fill (m/normalize pos 255))
    (q/with-translation pos
      (q/box 10))))


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
  (let [{:keys [tick tree-updater agents swarm-fn]} @state]
    (a/move agents config)
    ;;(tree-updater agents)
    ;;    (swarm-fn agents config)
    (a/bounce agents config)
  
    (reset! state
            (cond-> @state
              (zero? (mod tick 2)) (update :trail into (a/get-positions agents config))
              :default (update :tick inc)))))


(defn draw []
  (update-state! state)
  
  (q/with-translation (:pos config)
    (q/background 0)
    ;;    (draw-box (:size config))
    ;;    (draw-agents agents)
    (draw-trail (:trail @state))))




(defn ^:export create []
  (q/with-sketch (q/get-sketch-by-id "art")
    (if (:active @state)
      (do
        (q/exit)
        (swap! state update :active not))))
  
  (q/defsketch art
    :renderer :p3d
    :setup setup
    :draw draw
    ;;    :features [:no-start]
    :size (:window-size config)))


(defn ^:export toggle []
  (q/with-sketch (q/get-sketch-by-id "art")
    (if @state (q/start-loop) (q/no-loop)))
  
  (swap! state not))
