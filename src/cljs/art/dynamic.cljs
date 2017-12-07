(ns art.dynamic
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.vector :as v]
            [thi.ng.geom.spatialtree :as t]
            [art.agents :as a]
            [taoensso.timbre :refer [info]]))

(def agents (atom nil))

(def config {:agent-count 80
             :color [200 200 0]
             :window-size [768 560]
             :size [1000 500 500]
             :radius 70
             :cohesion 0.36
             :separation 0.38
             :alignment 0.5
             :max-vel 6
             :view-angle 250})


(defn setup []
  (let [tree (apply t/octree 0 0 0 (:size config))]
    (reset! agents (apply a/generate-agents
                          (mapv config [:agent-count :size])))
    (q/frame-rate 60)
    (q/stroke-weight 0.5)
    (q/stroke 180)
    (q/no-fill)
    (q/background 0)
    {:tree-updater (a/update-tree tree)
     :swarm-fn (a/swarm tree config)
     :agents @agents}))



(defn draw-box [[x y z]]
  (q/no-fill)  
  (q/with-translation [(/ x 2)  (/ y 2)  (/ z 2)]
    (q/box x y z)))


(defn draw-agents [agents]
  (apply q/fill (:color config))
  (doseq [a agents]
    (q/with-translation (:pos a)
      (q/box 10))))



(defn draw [state]
  (q/with-translation [-100 0 -800]
    (q/background 0)
    (draw-box (:size config))
    (draw-agents (:agents state))))



(defn update-state [{:keys [tree-updater agents swarm-fn] :as state}]
  (tree-updater agents)
  (update state :agents
          #(-> (swarm-fn %)
               (a/move config)
               (a/bounce config))))


(def state (atom false))


(defn ^:export create []
  (q/with-sketch (q/get-sketch-by-id "art")
    (if @state (q/exit)))
  
  (q/defsketch art
    :middleware [m/fun-mode m/navigation-3d]
    :renderer :p3d
    :setup setup
    :draw draw
    :update update-state
    ;;    :features [:no-start]
    :size (:window-size config)))


(defn ^:export toggle []
  (q/with-sketch (q/get-sketch-by-id "art")
    (if @state (q/start-loop) (q/no-loop)))
  
  (swap! state not))
