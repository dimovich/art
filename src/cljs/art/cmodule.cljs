(ns art.cmodule)


(def module (js/Agents))


(def c-init           (.cwrap module "init_agent_system" "*"
                              #js ["number" "number" "number" "number"
                                   "number" "number" "number" "number" "number"]))
(def c-update-config  (.cwrap module "update_agent_config" "*"
                              #js ["number" "number" "number" "number" "number"
                                   "number" "number" "number" "number" "number"]))
(def c-update-state   (.cwrap module "update_agent_system" "*" #js ["number"]))
(def c-count          (.cwrap module "get_agent_count" "number" #js ["number"]))
(def c-get            (.cwrap module "get_agent_component" "number" #js ["number" "number" "number"]))
(def c-agents-ptr     (.cwrap module "get_agents_pointer" "number" #js ["number"]))



(defn update-config
  [sys {:keys [speed cohesion separation alignment
               radius agent-count]
        [x y z] :size}]
  
  (c-update-config sys x y z agent-count cohesion separation alignment speed radius))



(defn update-state [state]
  (c-update-state (:sys state))
  state)



(defn init
  [{:keys [speed cohesion separation alignment
           radius agent-count]
    [x y z] :size}]
  
  (c-init x y z agent-count cohesion separation alignment speed radius))
