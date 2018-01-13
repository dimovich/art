(ns art.dynamic
  (:require [taoensso.timbre :refer [info]]
            [thi.ng.geom.core    :as g]
            [thi.ng.geom.vector  :as v]
            [thi.ng.math.core    :as m]
            [amalloy.ring-buffer :as r]
            [art.config :refer [config]]
            [art.agents :as a]
            [art.gl     :as agl]))



(defn setup [config]
  (let [size    (:size config)
        agents  (a/generate-agents config)
        trail   (r/ring-buffer (* 3 (:trail-size config)))
        canvas  (.getElementById js/document (:canvas config))]

    (set! (.-width canvas) (.-innerWidth js/window))
    (set! (.-height canvas) (* 0.9 (.-innerHeight js/window)))
  
    {:trail  (a/update-trail trail agents)
     :uuid   (.. (js/Date.) getTime)
     :agents agents
     :canvas canvas}))



(defn update-state [state]
  (let [{:keys [agents config]} state]
    ((juxt a/move a/swarm a/bounce) agents config)
    (update state :trail a/update-trail agents)))



(defn create! [state]
  (let [static (:static @state)]
    
    (->> (setup (:config @state))
         (merge {:static false})
         (swap! state merge))
    
    (agl/init-app-3d state)
    (agl/update-app-3d state update-state)

    (swap! state assoc :static static)))



(defn toggle! [state]
  (doto state
    (swap! update :active not)
    (swap! update :static not)))



(defn destroy! [state]
  (let [canvas (:canvas @state)]
    (set! (.-width canvas) 1)
    (set! (.-height canvas) 1)
    (swap! state select-keys
           [:active :static :config :cam])))



(defn restart! [state]
  (destroy! state)
  (create! state))
