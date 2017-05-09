(ns art.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [art.dynamic :as d]))


(q/defsketch example                
  :title "Oh so many grey circles"
  :middleware [m/fun-mode;; m/navigation-3d
               ]
;;  :renderer :p3d
  :setup d/setup           
  :draw d/draw              
  :size [323 200])
