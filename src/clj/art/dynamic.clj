(ns art.dynamic
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil.middlewares.bind-output :refer [bind-output]]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.vector :as v]
            [thi.ng.geom.spatialtree :as t]
            [art.agents :as a]
            [taoensso.timbre :refer [info]]))


(def config {:agent-count 100
             :color [200 200 0]
             :window-size [768 540]
             :size [1000 500 500]
             :radius 60
             :cohesion 0.27
             :separation 0.30
             :alignment 0.55
             :max-vel 6
             :view-angle 250})


(defn setup []
  (let [tree (apply t/octree 0 0 0 (:size config))]
    (q/frame-rate 60)
    (apply q/fill (:color config))
    (q/background 0)
    {:tree tree
     :agents (apply a/generate-agents
                    (mapv config [:agent-count :size]))}))



(defn draw-box [[x y z]]
  (q/push-style)
  (q/stroke-weight 0.5)
  (q/stroke 200)
  (q/no-fill)
  (q/with-translation [(/ x 2)  (/ y 2)  (/ z 2)]
    (q/box x y z))
  (q/pop-style))


(defn draw-agents [agents]
  (doseq [a agents]
    (q/with-translation (:pos a)
      (q/box 10))))



(defn draw [state]
  (q/with-translation [0 0 -800]
    (q/background 0)
    (draw-agents (:agents state))
    (draw-box (:size config))))



(defn update-state [state]
  (update state :agents
          #(-> %
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
