(ns art.gl
  (:require
   ;;[taoensso.timbre :refer [info]]
   [art.config :refer [config]]
   [thi.ng.math.core :as m :refer [PI HALF_PI TWO_PI]]
   [thi.ng.color.core :as col]
   [thi.ng.geom.gl.core :as gl]
   [thi.ng.geom.gl.webgl.constants :as glc]
   [thi.ng.geom.gl.webgl.animator :as anim]
   [thi.ng.geom.gl.shaders :as sh]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.matrix :as mat :refer [M44]]
   [thi.ng.geom.quaternion :as q]
   [thi.ng.geom.gl.arcball :as arc]
   [thi.ng.geom.aabb :as aa]
   [thi.ng.geom.gl.glmesh :as glm]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.gl.shaders.basic :as basic]
   [art.cmodule :as c]))



(def shader-spec
  {:vs "void main() {
          gl_Position = proj * view * model * vec4(position, 1.0);
          gl_PointSize = 7.0 - min(gl_Position.w, 4.0);
          p = normalize(position);
          vCol = vec4(p.x, p.y, p.z, 1);
        }"
   :fs "void main() {
          gl_FragColor = vCol;
        }"
   :uniforms {:model      [:mat4 M44]
              :view       :mat4
              :proj       :mat4}
   :attribs  {:position   :vec3}
   :varying  {:vCol       :vec4
              :p :vec3}
   :state    {:depth-test true}})



(defn attrib-buffer-view
  [ptr stride num]
  (js/Float32Array. (.-buffer (aget c/module "HEAPU8")) ptr (* stride num)))



(defn update-attrib-buffer
  [gl scene attrib ptr stride num]
  (.bindBuffer gl glc/array-buffer
               (get-in scene [:particles :attribs attrib :buffer]))
  (.bufferData gl glc/array-buffer
               (attrib-buffer-view ptr stride num)
               glc/dynamic-draw))



(defn init-arcball! [state el vrect]
  (swap! state assoc :cam
         (-> (arc/arcball {:init (m/normalize (q/quat 0.0 0.707 0.707 0))})
             (arc/resize (g/width vrect) (g/height vrect))))
  (doto el
    (.addEventListener
     "mousedown"
     (fn [e]
       (swap! state assoc :prev-static (:static @state))
       (swap! state assoc :static false)
       (doto state
         (swap! assoc :mouse-down true)
         (swap! update :cam arc/down (.-clientX e) (.-clientY e)))))
    (.addEventListener
     "mouseup"
     (fn [e]
       (swap! state assoc :static (:prev-static @state))
       (swap! state assoc :mouse-down false)))
    (.addEventListener
     "mousemove"
     (fn [e]
       (when (:mouse-down @state)
         (swap! state update :cam arc/drag (.-clientX e) (.-clientY e)))))))

;;guntt-chart

(defn init-app-3d [state]
  (let [agent-count  (-> @state :config :agent-count)
        agents       (:agents @state)
        dom          (:canvas @state)
        gl           (gl/gl-context dom)
        view         (gl/get-viewport-rect gl)

        uniforms     {:view (mat/look-at (vec3 0 1 0) (vec3 0 1 0) (vec3 0 1 0))
                      :proj (mat/perspective 50 view 0.1 100)}
        
        box          (-> (aa/aabb 2)
                         (g/center)
                         (g/as-mesh
                          {:mesh    (glm/indexed-gl-mesh 12 #{:col})
                           :attribs {:col (->> (repeatedly #(identity [(rand) (rand) (rand)]))
                                               (take 6)
                                               (map col/rgba)
                                               (attr/const-face-attribs))}})
                         (gl/as-gl-buffer-spec {})
                         (assoc :shader (sh/make-shader-from-spec gl (basic/make-shader-spec-3d true)))
                         (gl/make-buffers-in-spec gl glc/static-draw)
                         (update :uniforms merge uniforms))

        ;; 10 num of floats
        ;; 40 num of bytes
        ;; 3  num of vertices
        particles    (-> {:attribs  {:position {:data (attrib-buffer-view agents 10 agent-count)
                                                :size   3
                                                :stride 40}}
                          :mode     glc/points
                          :num-vertices agent-count}
                         (gl/make-buffers-in-spec gl glc/dynamic-draw)
                         (assoc :shader (sh/make-shader-from-spec gl shader-spec))
                         (update :uniforms merge uniforms))]
    
    (when-not (:cam @state)
      (init-arcball! state dom view))
    
    (swap! state merge
           {:gl           gl
            :translate    (vec3 -0.5)
            :scene        {:particles particles
                           :container box}})))




(defn update-app-3d
  [state]
  (let [start-uuid (:uuid @state)]
    (anim/animate
     (fn [t frame]
       (when (:active @state)
         (swap! state c/update-state))

       (let [{:keys [gl scene cam translate uuid static agents]
              {:keys [agent-count]} :config} @state]
         (when (= uuid start-uuid)
           (when-not static
             (update-attrib-buffer gl scene :position agents 10 agent-count)
             (doto gl
               (gl/clear-color-and-depth-buffer col/WHITE 1)
             
               (gl/draw-with-shader
                (-> (:container scene)
                    (assoc-in [:uniforms :model]
                              (-> (arc/get-view cam)
                                  (g/scale 0.1)))))
           
               (gl/draw-with-shader
                (-> (:particles scene)
                    (assoc :num-vertices agent-count)
                    (assoc-in [:uniforms :model]
                              (-> (arc/get-view cam)
                                  (g/translate translate)
                                  (g/scale 0.1)))))))
           true))))))

