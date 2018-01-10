(ns art.core
  (:require [dommy.core  :as d]
            [art.dynamic :as art]
            [taoensso.timbre :refer [info]]))



(defn init []
  (let [player (d/sel1 :.control-wrapper)
        closer (d/sel1 :.close-wrapper)]
    
    (d/listen! player :click
               (fn [_]
                 (if (and (d/has-class? player :paused)
                          (d/has-class? closer :hidden))
                   (do
                     (art/create!)
                     (d/remove-class! closer :hidden))
                   
                   (art/toggle!))
                 
                 (d/toggle-class! player :playing)
                 (d/toggle-class! player :paused)))

    
    (d/listen! closer :click
               (fn [_]
                 (art/destroy!)
                 (d/remove-class! player :playing)
                 (d/add-class! player :paused)
                 (d/add-class! closer :hidden)))))


(defn ^:export main []
  (init))

