
(def config {:agent-count 100
             :color [200 200 0]
             :window-size [768 560]
             :size [1000 500 500]
             :radius 60
             :cohesion 0.27
             :separation 0.30
             :alignment 0.55
             :max-vel 6
             :view-angle 250})


(def agents (generate-agents 50 [500 500 500]))

agents

(def ps (->> agents
             (map (fn [{:keys [pos vel]}]
                    (-> (p/particle pos)
                        (p/add-force vel))))))


(defn update-accelerator
  [accel]
  (fn [physics _]
    (reduce
     #(g/add-point % (p/position %2) %2)
     (g/clear! accel)
     (:particles physics))))



(defn attract!
  [p q rsq strength delta]
  (let [d (g/- p (phys/position q))
        l (+ (g/mag-squared d) 1e-6)]
    (if (< l rsq)
      (phys/add-force
       q (g/* d (/ (* (- 1.0 (/ l rsq)) (* strength delta))
                   (Math/sqrt l)))))))




(defn swarm [p q r]
  (mapv
   (fn [a]
     (let [others (->> (t/select-with-sphere tree (:pos a) radius)
                       (remove #{a})
                       (distance a)
                       (remove #(zero? (:dist %))))]
       (if (empty? others)
         a
         (let [force (reduce
                      m/+
                      (v/vec3)
                      (remove
                       nil?
                       [(some-> (swarm-separate a others radius)
                                (m/* separation)) 
                        (some-> (swarm-align a others radius)
                                (m/* alignment))
                        (some-> (swarm-cohere a others radius)
                                (m/* cohesion))]))
               acc (m/+ (:acc a) force)]
           (assoc a :acc acc)))))
   agents))



(defn agent-swarm
  [tree {:keys [radius cohesion
                separation alignment
                view-angle max-vel size]}]
  (fn [p delta]
    (let [p' (p/position p)]
      (loop [neighbors (t/select-with-sphere tree p' radius)]
        (when-let [n (first neighbors)]
          (if-not (= p n) (swarm p n radius))
          (recur (next neighbors)))))))



(phys/physics
 {:particles ps
  :behaviors {:g (phys/gravity (:gravity state))
              :f (agent-swarm tree config)}
  :listeners {:iter (update-tree tree)}})

