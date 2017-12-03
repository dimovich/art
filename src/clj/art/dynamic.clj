(ns art.dynamic
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil.middlewares.bind-output :refer [bind-output]]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.core.vector :as v]
            [thi.ng.geom.spatialtree :as t]
            [art.agents :as a]
            [taoensso.timbre :refer [info]]))


(def config {:agent-count 100
             :window-size [740 560]
             :size [500 500 500]
             :radius 80
             :cohesion 0.59
             :separation 0.58
             :alignment 0.15
             :max-vel 2
             :view-angle 250})


(defn setup []
  (let [tree (apply t/octree 0 0 0 (:size config))]
    ;; (q/smooth)
    (q/frame-rate 60)
    (q/background 0)
    {:tree tree
     :agents (apply a/generate-agents
                    (mapv config [:agent-count :size]))}))



(defn draw-box [[x y z]]
  (q/push-style)
  (q/push-matrix)
  (q/stroke-weight 0.5)
  (q/stroke 200)
  (q/no-fill)
  (q/translate (/ x 2)  (/ y 2)  (/ z 2))
  (q/box x y z)
  (q/pop-matrix)
  (q/pop-style))


(defn draw-agents [agents]
  (let [color [0 255 0]]
    (q/push-style)
    (q/color-mode :rgb 255)
    (apply q/stroke color)
    (q/stroke-weight 10)
    (q/begin-shape :points)
    (q/no-fill)
    (doseq [a agents]
      (apply q/vertex (:pos a)))
    (q/end-shape)
    (q/pop-style)))



(defn draw [state]
  (q/background 0)
  (draw-agents (:agents state))
  (draw-box (:size config)))



(defn update-state [state]
  (update state :agents #(-> %
                             (a/swarm config)
                             (a/move config)
                             (a/bounce config))))


(defn run []
  (q/defsketch example
    :middleware [m/fun-mode m/navigation-3d]
    :renderer :p3d
    :setup setup
    :draw draw
    :update update-state
    :size (:window-size config)))

#_(
   (run)

   ) 

