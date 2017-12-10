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
             :trail-size 180})



(defonce state (atom {:active false}))


(defn setup []
  (let [tree (apply t/octree 0 0 0 (:size config))
        agents (-> (->> [:agent-count :size]
                        (mapv config)
                        (apply a/generate-agents))
                   (a/agents->array [:pos :acc :vel]))]
    (q/frame-rate 60)
    (q/stroke-weight 0.5)
    (q/stroke 180)
    (q/no-fill)
    (q/background 0)
    (swap! state merge {:tick 0
                        :tree-updater (a/update-tree tree)
                        :swarm-fn (a/swarm tree)
                        :trail (r/ring-buffer (:trail-size config))
                        :agents {:data agents
                                 :count (:agent-count config)}})))



(defn draw-box [[x y z]]
  ;;(q/no-fill)
  (q/stroke-weight 0.5)
  ;;(q/stroke 33)
  (q/with-translation [(/ x 2)  (/ y 2)  (/ z 2)]
    (q/box x y z)))


(defn draw-agents [{{:keys [pos]} :data
                    count :count}]
  (q/no-stroke)
  ;;(q/stroke-weight 3)
  ;;(q/begin-shape :points)
  (loop [idx 0]
    (when (< idx count)
      (let [apos (a/get-vec3 pos idx)]
        (apply q/fill (m/normalize apos 255))
        ;;(apply q/stroke (m/normalize apos 255))
        ;;(apply q/vertex apos)
        (q/with-translation apos
          (q/box 10)))
      (recur (inc idx))))
  ;;(q/end-shape)
  )


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
    (swap! state update :trail into (a/get-positions agents))
    #_(reset! state
              (cond-> @state
                (zero? (mod tick 1)) (update :trail into (a/get-positions agents))
                :default (update :tick inc)))
    
    
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
    (draw-trail (:trail @state))
    ))




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
