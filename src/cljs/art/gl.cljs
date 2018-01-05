(ns art.gl
  (:require
   [taoensso.timbre :refer [info]]
   [art.agents :as a]
   [art.config :refer [config]]
   [thi.ng.math.core :as m :refer [PI HALF_PI TWO_PI]]
   [thi.ng.color.core :as col]
   [thi.ng.typedarrays.core :as arrays]
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
   
   ;;[thi.ng.geom.gl.buffers :as buf]
   ;;[thi.ng.geom.gl.utils :as glu]
   [thi.ng.geom.gl.glmesh :as glm]
   ;;[thi.ng.geom.gl.camera :as cam]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.geom.gl.shaders.basic :as basic]
   ;;[thi.ng.glsl.core :as glsl :include-macros true]
   ;;[thi.ng.geom.plane :as pl]
   ;;[thi.ng.geom.gl.shaders.phong :as phong]
   ;;[thi.ng.geom.mesh.io :as mio]
   ))



(defn init-arcball
  [state el vrect]
  (swap! state assoc :cam
         (-> (arc/arcball {:init (m/normalize (q/quat 0.0 0.707 0.707 0))})
             (arc/resize (g/width vrect) (g/height vrect))))
  (doto el
    (.addEventListener
     "mousedown"
     (fn [e]
       (doto state
         (swap! assoc :mouse-down true)
         (swap! update :cam arc/down (.-clientX e) (.-clientY e)))))
    (.addEventListener
     "mouseup"
     (fn [e] (swap! state assoc :mouse-down false)))
    (.addEventListener
     "mousemove"
     (fn [e]
       (when (:mouse-down @state)
         (swap! state update :cam arc/drag (.-clientX e) (.-clientY e)))))))




(def shader-spec
  {:vs "void main() {
    p = normalize(position);
    vCol = vec4(p.x, p.y, p.z, 1);
    gl_Position = proj * view * model * vec4(position, 1.0);
    gl_PointSize = 6.0 - gl_Position.w*0.5;
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
  [data]
  (arrays/float32 (apply concat data)))



(defn update-attrib-buffer
  [gl scene attrib data]
  (.bindBuffer gl glc/array-buffer
               (get-in scene [:particles :attribs attrib :buffer]))
  (.bufferData gl glc/array-buffer
               (attrib-buffer-view data)
               glc/dynamic-draw))



(defn init-app-3d
  [state dom]
  (let [positions    (a/get-positions (:agents @state))
        gl           (gl/gl-context dom)
        view         (gl/get-viewport-rect gl)

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
                         (update :uniforms merge
                                 {:view (mat/look-at (vec3 0 1 0) (vec3 0 1 0) (vec3 0 1 0))
                                  :proj (mat/perspective 50 view 0.1 100)}))
        
        particles    (-> {:attribs  {:position {:data (attrib-buffer-view positions)
                                                :size   3
                                                :stride 12}}
                          :mode     glc/points
                          :num-vertices (count positions)}
                         (gl/make-buffers-in-spec gl glc/dynamic-draw)
                         (assoc :shader (sh/make-shader-from-spec gl shader-spec))
                         (update :uniforms merge
                                 {:view (mat/look-at (vec3 0 1 0) (vec3 0 1 0) (vec3 0 1 0))
                                  :proj (mat/perspective 50 view 0.1 100)}))]
    
    (init-arcball state dom view)
    
    (swap! state merge
           {:gl           gl
            :translate    (vec3 -0.5)
            :scene        {:particles particles
                           :container box}})))




(defn update-app-3d
  [state update-fn]
  (anim/animate
   (fn [t frame]
     (when (:active @state)
       (swap! state update-fn)
       (let [{:keys [gl scene trail cam translate]} @state]
         (update-attrib-buffer gl scene :position trail)
         (doto gl
           (gl/clear-color-and-depth-buffer col/WHITE 1)
           (gl/draw-with-shader
            (-> (:container scene)
                (assoc-in [:uniforms :model]
                          (-> (arc/get-view cam)
                              (g/scale 0.1)))))
           
           (gl/draw-with-shader
            (-> (:particles scene)
                (assoc :num-vertices (count trail))
                (assoc-in [:uniforms :model]
                          (-> (arc/get-view cam)
                              (g/translate translate)
                              (g/scale 0.08)
                              )))))))
     true)))

