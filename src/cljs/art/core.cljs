(ns art.core
  (:require [dommy.core  :as d]
            [art.cmodule :as c]
            [art.dynamic :as art]
            [art.config  :refer [config]]
            [taoensso.timbre :refer [info]]))


(def state (atom {:active true
                  :config config}))


(defn set-range-input [state max k]
  (when-let [el (d/sel1 (->> k name (str "#") keyword))]
    (let [val (get-in @state [:config k])]
      
      (set! (.-value el) (* (/ val max) 100))
      
      (d/listen!
       el :input
       (fn [e]
         (when-let [v (.-value (.-target e))]
           (->> v
                (* 0.01 max)
                (swap! state assoc-in [:config k]))
           (c/update-config (:sys @state) (:config @state))))))))



(defn init [state]
  (let [player    (d/sel1 :.control-wrapper)
        refresher (d/sel1 :.refresh-wrapper)]
    
    (d/listen! player :click
               (fn [_]
                 (if (and (d/has-class? player :paused)
                          (d/has-class? refresher :hidden))
                   (do
                     (art/create! state)
                     (d/remove-class! refresher :hidden))
                   
                   (art/toggle! state))
                 
                 (d/toggle-class! player :playing)
                 (d/toggle-class! player :paused)))


    (d/listen! refresher :click #(art/restart! state))

    (->> [:cohesion :separation :alignment]
         (map (partial set-range-input state
                       (get-in @state [:config :max-swarm])))
         doall)

    (set-range-input state (get-in @state [:config :max-speed]) :speed)
    (set-range-input state (get-in @state [:config :size 0]) :radius)))



(defn ^:export main []
  (init state))

