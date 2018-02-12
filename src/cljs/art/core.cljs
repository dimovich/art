(ns art.core
  (:require [dommy.core  :as d]
            [art.cmodule :as c]
            [art.dynamic :as art]
            [art.config  :refer [config]]
            ;;[taoensso.timbre :refer [info]]
            ))


(def state (atom {:active true
                  :config config}))


(defn set-range-input
  ([state k min max]
   (set-range-input state k min max (float (/ (- max min) 100))))
  ([state k min max step]
   (when-let [el (d/sel1 (->> k name (str "#") keyword))]
     (let [val (get-in @state [:config k])]

       (set! (.-min el) min)
       (set! (.-max el) max)
       (set! (.-step el) step)
       (set! (.-value el) val)
      
       (d/listen!
        el :input
        (fn [e]
          (when-let [v (.-value (.-target e))]
            (swap! state assoc-in [:config k] v)
            (c/update-config (:sys @state) (:config @state)))))))))



(defn init [state]
  (let [player    (d/sel1 :.control-wrapper)
        refresher (d/sel1 :.refresh-wrapper)
        config    (:config @state)]
    
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
         (map #(set-range-input
                state %
                0 (:max-swarm config)))
         doall)

    (set-range-input state :speed  0 (:max-speed config))
    (set-range-input state :radius 0 (get-in config [:size 0]))
    (set-range-input state :trail-size 1 (:max-trail config))
    (set-range-input state :agent-count 1 (:max-agents config))))



(defn ^:export main []
  (init state))

