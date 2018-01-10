(ns art.core
  (:require [dommy.core  :as d]
            [art.dynamic :as art]
            [taoensso.timbre :refer [info]]))


(def state (atom {:active true}))


(defn init []
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


    (d/listen! refresher :click #(art/restart! state))))



(defn ^:export main []
  (init))

