(ns art.config)


(def config {:canvas "art"
             :agent-count 50
             :size [13 13 13]
             :radius 4
             :cohesion 0.039
             :separation 0.04
             :alignment 0.06
             :max-vel 0.2
             :trail-size (* 3 80)})
