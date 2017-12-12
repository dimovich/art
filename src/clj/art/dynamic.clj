(ns art.dynamic
  (:require [quil.core :as q]
            [quil.middleware :as qm]
            [amalloy.ring-buffer :as r]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.vector :as v]
            [thi.ng.math.core :as m]
            [thi.ng.geom.spatialtree :as t]
            [art.agents :as a]
            [taoensso.timbre :refer [info]]))

(def state (atom {:active false}))

(def config {:agent-count 200
             :color [200 200 0]
             :window-size [900 600]
             :size [800 800 500]
             :pos [0 200 -600]
             :radius 60
             :cohesion 0.39
             :separation 0.4
             :alignment 0.6
             :max-vel 6
             :trail-size 300})


(defn setup []
  (let [tree (apply t/octree 0 0 0 (:size config))
        agents (a/generate-agents config)]
    (q/frame-rate 60)
    (q/stroke-weight 3)
    (q/stroke 180)
    (q/no-fill)
    (q/background 0)
    
    {:tick 0
     :angle 0
     :tree-fn (a/update-tree tree)
     :swarm-fn (a/swarm tree config)
     :positions-fn (a/get-positions)
     :trail (r/ring-buffer (:trail-size config))
     :agents agents}))



(defn draw-box [[x y z]]
  (q/stroke-weight 0.1)
  (q/with-translation [(/ x 2)  (/ y 2)  (/ z 2)]
    (q/box x y z)))


(defn draw-agents [agents]
  (q/stroke-weight 3)
  (q/begin-shape :points)
  (let [size (count agents)]
    (loop [idx 0]
      (when (< idx size)
        (let [a (get agents idx)
              pos (.-pos a)
              color (m/normalize pos 255)]
          (apply q/stroke color)
          (apply q/vertex pos))
        (recur (inc idx)))))
  (q/end-shape))


(defn draw-trail [trail]
  
  (q/begin-shape :points)
  (loop [trail trail]
    (when-let [p (first trail)]
      (apply q/stroke (m/normalize p 255))
      (apply q/vertex p)
      (recur (next trail))))
  (q/end-shape))


(defn update-state [state]
  ;;  (info (:navigation-3d state))
  (let [{:keys [agents tree-fn swarm-fn positions-fn tick]} state]
    (tree-fn agents)
    (swarm-fn agents config)
    (a/move agents config)
    (a/bounce agents config)
    ;;(update state :trail into (positions-fn agents))
    state
    ))


(defn draw [state]
  ;;q/with-translation (:pos config)
  (q/background 0)
  ;;(draw-box (:size config))
  (draw-agents (:agents state))
  ;;(draw-trail (:trail state))
  )




(defn create []
  #_(q/with-sketch (q/get-sketch-by-id "art")
      (if (:active @state)
        (q/exit)
        (swap! state update :active not)))
  
  (q/defsketch art
    :renderer :opengl
    :middleware [qm/fun-mode qm/navigation-3d]
    :navigation-3d {:position [340.06592727916393 105.44126878160894 991.7662366785296]
                    :straight [0.08818531557111595 0.35380829942837455 -0.8719076123592358]
                    :up [-0.008166460209455039 0.9551572896947682 0.14789964782999468]}
    
    :setup setup
    :draw draw
    :update update-state
    :size (:window-size config)))


(defn toggle []
  #_(q/with-sketch (q/get-sketch-by-id "art")
      (if (:active @state)
        (q/no-loop)
        (q/start-loop)))
  
  (swap! state update :active not))
