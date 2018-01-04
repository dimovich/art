(ns art.gl
  (:require-macros
   [thi.ng.math.macros :as mm])
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
   [thi.ng.geom.gl.buffers :as buf]
   [thi.ng.geom.gl.shaders :as sh]
   [thi.ng.geom.gl.utils :as glu]
   [thi.ng.geom.gl.glmesh :as glm]
   [thi.ng.geom.gl.camera :as cam]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.matrix :as mat :refer [M44]]
   [thi.ng.geom.attribs :as attr]
   [thi.ng.glsl.core :as glsl :include-macros true]
   [thi.ng.geom.plane :as pl]
   [thi.ng.geom.gl.shaders.phong :as phong]
   [thi.ng.geom.mesh.io :as mio]
   [thi.ng.geom.quaternion :as q]
   [thi.ng.geom.gl.arcball :as arc]))



(defn init-arcball
  [state el vrect]
  (swap! state assoc :cam
         (-> (arc/arcball {:init (m/normalize (q/quat 0.0 0.707 0.707 0))
                           :center (m/div (v/vec3 (:size config)) 2)})
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
    vCol = vec4(0.2, 0.2, 0.2, 0.8);
    gl_Position = proj * view * model * vec4(position, 1.0);
    gl_PointSize = 8.0 - gl_Position.w * 1.1;
    }"
   :fs "void main() {
    gl_FragColor = vCol;
    }"
   :uniforms {:model      [:mat4 M44]
              :view       :mat4
              :proj       :mat4}
   :attribs  {:position   :vec3}
   :varying  {:vCol       :vec4}
   :state    {:depth-test false
;;              :blend      true
;;              :blend-fn   [glc/src-alpha glc/one]
              }})





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

        shader     (sh/make-shader-from-spec gl phong/shader-spec)
        size       20
        ground-y   -1.55
        ground     (pl/plane-with-point (vec3 0 ground-y 0) v/V3Y)
        back       (pl/plane-with-point (vec3 0 0 (* -0.5 size)) v/V3Z)
        planes     (-> (g/as-mesh back {:mesh (glm/indexed-gl-mesh 4 #{:fnorm}) :size size})
                       (g/translate (vec3 0 (+ (* 0.5 size) ground-y) 0))
                       (g/into (g/as-mesh ground {:size size}))
                       (gl/as-gl-buffer-spec {})
                       (assoc :uniforms {:proj          (mat/perspective 70 view 0.1 50)
                                         :view          (mat/look-at (v/vec3 0 0 1) (v/vec3) v/V3Y)
                                         :lightPos      (vec3 0.1 0 1)
                                         :ambientCol    0x000011
                                         :diffuseCol    0x0033ff
                                         :specularCol   0xffffff
                                         :shininess     100
                                         :wrap          0
                                         :useBlinnPhong true}
                              :shader shader)
                       (gl/make-buffers-in-spec gl glc/static-draw))
        
        
        particles    (-> {:attribs  {:position {:data (attrib-buffer-view positions)
                                                :size   3
                                                :stride 12}}
                          :num-vertices (count positions)
                          :mode     glc/points}
                         (gl/make-buffers-in-spec gl glc/dynamic-draw)
                         (assoc :shader (sh/make-shader-from-spec gl shader-spec))
                         (update :uniforms merge
                                 #_{:proj          (mat/perspective 90 view 0.1 100)
                                    :view          (mat/look-at (v/vec3 0 0 1) (v/vec3) v/V3Y)}
                                 {:view (mat/look-at (vec3 0 1 0) (vec3 0 1 0) (vec3 0 1 0))
                                  :proj (mat/perspective 60 view 0.1 50)}))]

    
    (init-arcball state dom view)
    
    (swap! state merge
           {:gl           gl
            :scene        {:particles particles
                           :planes planes}})))




(defn update-app-3d
  [state update-fn]
  (anim/animate
   (fn [t frame]
     (swap! state update-fn)
     (let [{:keys [gl scene trail cam active]} @state]
       (update-attrib-buffer gl scene :position trail)
       (doto gl
         ;;(gl/set-viewport vrect)
         (gl/clear-color-and-depth-buffer col/WHITE 1)
         
         (gl/draw-with-shader
          (-> (:planes scene)
              (assoc-in [:uniforms :model] (g/scale (arc/get-view (:cam @state))
                                                    0.1))))
         (gl/draw-with-shader
          (-> (:particles scene)
              (assoc :num-vertices (count trail))
              (assoc-in [:uniforms :model] (g/scale (arc/get-view cam)
                                                    0.1)))))
       active))))













#_(defn ^:export demo
    [state]
    (let [gl    (gl/gl-context "art")
          vrect (gl/get-viewport-rect gl)
          shader     (sh/make-shader-from-spec gl phong/shader-spec)
          size       20
          ground-y   -1.55
          ;;ground     (pl/plane-with-point (vec3 0 ground-y 0) v/V3Y)
          back       (pl/plane-with-point (vec3 0 0 (* -0.5 size)) v/V3Z)
          planes     (-> (g/as-mesh back {:mesh (glm/indexed-gl-mesh 4 #{:fnorm}) :size size})
                         (g/translate (vec3 0 (+ (* 0.5 size) ground-y) 0))
                         ;;             (g/into (g/as-mesh ground {:size size}))
                         (gl/as-gl-buffer-spec {})
                         (assoc :uniforms {:proj          (mat/perspective 70 vrect 0.1 50)
                                           :view          (mat/look-at (v/vec3 0 0 1) (v/vec3) v/V3Y)
                                           :lightPos      (vec3 0.1 0 1)
                                           :ambientCol    0x000011
                                           :diffuseCol    0x0033ff
                                           :specularCol   0xffffff
                                           :shininess     100
                                           :wrap          0
                                           :useBlinnPhong true}
                                :shader shader)
                         (gl/make-buffers-in-spec gl glc/static-draw))]
      (init-arcball state (.getElementById js/document "art") vrect)
      (anim/animate
       (fn [t frame]
         (doto gl
           (gl/set-viewport vrect)
           (gl/clear-color-and-depth-buffer col/BLACK 1)
           (gl/draw-with-shader
            (assoc-in planes [:uniforms :model] (arc/get-view (:cam @state)))))
         true))))
