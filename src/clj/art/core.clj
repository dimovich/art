(ns art.core
  (:require [taoensso.timbre :as timbre :refer [info]]
            [art.dynamic :as d])
  (:gen-class))

#_(timbre/set-config!
   {:level :info
    :output-fn (fn [{:keys [timestamp_ level msg_]}]
                 (str
                  (second (clojure.string/split (force timestamp_) #" ")) " "
                  (force msg_)
                  "\n\n"))
    :appenders {:println (timbre/println-appender {:stream :auto})}})


