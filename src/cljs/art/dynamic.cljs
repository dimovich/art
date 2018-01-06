(ns art.dynamic
  (:require [amalloy.ring-buffer :as r]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.vector :as v]
            [thi.ng.math.core :as m]
            [thi.ng.geom.spatialtree :as t]
            [thi.ng.typedarrays.core :as ta]
            [art.agents :as a]
            [taoensso.timbre :refer [info]]
            [art.gl :as agl]
            [art.config :refer [config]]))

(def state (atom {:active true}))


(defn setup [config]
  (let [tree   (apply t/octree 0 0 0 (:size config))
        agents (a/generate-agents config)
        trail  (r/ring-buffer (:trail-size config))
        canvas (.getElementById js/document (:canvas config))]

    (set! (.-width canvas) (* 0.9 (.-innerWidth js/window)))
    (set! (.-height canvas) (* 0.9 (.-innerHeight js/window)))
  
    {:tree-fn (a/update-tree tree)
     :swarm-fn (a/swarm tree config)
     :trail (a/update-trail trail agents)
     :agents agents
     :canvas canvas
     :active true
     :uuid (inc (:uuid @state))}))



(defn update-state [state]
  (let [{:keys [agents tree-fn swarm-fn]} state]
    (a/move agents config)
    ;;(tree-fn agents)
    ;;(a/create-tree agents config)
    (swarm-fn agents config)
    (a/bounce agents config)
    (update state :trail a/update-trail agents)))



(defn ^:export create []
  (reset! state (setup config))
  (agl/init-app-3d state)
  (agl/update-app-3d state update-state))



(defn ^:export toggle []
  (swap! state update :active not))
