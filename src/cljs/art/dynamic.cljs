(ns art.dynamic
  (:require ;;[taoensso.timbre :refer [info]]
            [art.config :refer [config]]
            [art.gl     :as agl]
            [art.cmodule :as c]))



(defn setup [config]
  (let [size    (:size config)
        sys     (c/init config)
        canvas  (.getElementById js/document (:canvas config))]

    (set! (.-width canvas) (.-innerWidth js/window))
    (set! (.-height canvas) (* 0.9 (.-innerHeight js/window)))
  
    {:uuid   (.. (js/Date.) getTime)
     :sys    sys
     :canvas canvas}))


(defn create! [state]
  (let [static (:static @state)]
    
    (->> (setup (:config @state))
         (merge {:static false})
         (swap! state merge))
    
    (agl/init-app-3d state)
    (agl/update-app-3d state)

    (swap! state assoc :static static)))



(defn toggle! [state]
  (doto state
    (swap! update :active not)
    (swap! update :static not)))



(defn destroy! [state]
  (let [canvas (:canvas @state)]
    (set! (.-width canvas) 1)
    (set! (.-height canvas) 1)
    (c/c-destroy-agent-system (:sys @state))
    (swap! state select-keys
           [:active :static :config :cam])))



(defn restart! [state]
  (destroy! state)
  (create! state))
