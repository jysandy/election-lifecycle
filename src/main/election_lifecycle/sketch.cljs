(ns election-lifecycle.sketch
  (:require [quil.core :as q]
            [election-lifecycle.vertex-buffer :as vb]
            [election-lifecycle.utils :as u]
            [election-lifecycle.polygon :as polygon]
            [election-lifecycle.particles :as particles]
            [election-lifecycle.vector :as vector]
            [election-lifecycle.tentacle :as tentacle]
            [election-lifecycle.config :as c]))

(def x first)
(def y second)

(defonce tie-texture (atom nil))
(defonce gradient-shader (atom nil))

(defn- init-tentacles
  []
  (let [offscreen-origin-offset 30
        top-origins             (map #(vector % (- (- (/ c/canvas-height 2))
                                                   offscreen-origin-offset))
                                     (range (+ (- (/ c/canvas-width 2))
                                               150)
                                            (/ c/canvas-width 2)
                                            300))
        bottom-origins          (map (fn [[x y]]
                                       [x (+ y
                                             c/canvas-height
                                             (* 2 offscreen-origin-offset))])
                                     top-origins)
        left-origins            (map #(vector (- (- (/ c/canvas-width 2))
                                                 offscreen-origin-offset) %)
                                     (range -300 (/ c/canvas-height 2) 150))
        right-origins           (map (fn [[x y]]
                                       [(+ x
                                           c/canvas-width
                                           (* 2 offscreen-origin-offset)) y])
                                     left-origins)]
    (doseq [origin (concat top-origins bottom-origins left-origins right-origins)]
      (vb/add-shape! (tentacle/generate-tentacle origin)))))

(defn- tie-top-vertices [centre-vertex size]
  (let [start-vertex [(- (x centre-vertex) (* 10 size)) (- (y centre-vertex) (* 25 size))]]
    [start-vertex
     [(+ (* 20 size) (x start-vertex)) (y start-vertex)]
     [(+ (* 25 size) (x start-vertex)) (- (y start-vertex) (* 10 size))]
     [(- (x start-vertex) (* 5 size)) (- (y start-vertex) (* 10 size))]]))

(defn- tie-top
  [centre-vertex size]
  (-> (polygon/make-polygon
        (tie-top-vertices centre-vertex size))
      (assoc-in [:meta :category] :tie-top)
      (assoc :texture @tie-texture)
      (assoc :animation {:current nil
                         :steps   {:sequence [{:target-vertices-fn (constantly (tie-top-vertices centre-vertex (+ 0.5 size)))
                                               :duration           75} ; grow
                                              {:target-vertices-fn (constantly (tie-top-vertices centre-vertex (+ 0.5 size)))
                                               :duration           75} ; pause
                                              {:target-vertices-fn (constantly (tie-top-vertices centre-vertex (+ 1 size)))
                                               :duration           75} ; grow
                                              {:target-vertices-fn (constantly (tie-top-vertices centre-vertex size))
                                               :duration           150} ; shrink
                                              {:target-vertices-fn (constantly (tie-top-vertices centre-vertex size))
                                               :duration           500} ; pause
                                              ]
                                   :repeat   true}})))

(defn- tie-bottom-vertices [centre-vertex size]
  (let [start-vertex [(- (x centre-vertex) (* 10 size)) (- (y centre-vertex) (* size 25))]]
    [start-vertex
     [(+ (* 20 size) (x start-vertex)) (y start-vertex)]
     [(+ (* 25 size) (x start-vertex)) (+ (* 80 size) (y start-vertex))]
     [(+ (* 10 size) (x start-vertex)) (+ (* 90 size) (y start-vertex))]
     [(- (x start-vertex) (* 5 size)) (+ (* 80 size) (y start-vertex))]]))

(defn- tie-bottom [centre-vertex size]
  (-> (polygon/make-polygon
        (tie-bottom-vertices centre-vertex size))
      (assoc-in [:meta :category] :tie-bottom)
      (assoc :texture @tie-texture)
      (assoc :animation {:current nil
                         :steps   {:sequence [{:target-vertices-fn (constantly (tie-bottom-vertices centre-vertex (+ 0.5 size)))
                                               :duration           75} ; grow
                                              {:target-vertices-fn (constantly (tie-bottom-vertices centre-vertex (+ 0.5 size)))
                                               :duration           75} ; pause
                                              {:target-vertices-fn (constantly (tie-bottom-vertices centre-vertex (+ 1 size)))
                                               :duration           75} ; grow
                                              {:target-vertices-fn (constantly (tie-bottom-vertices centre-vertex size))
                                               :duration           150} ; shrink
                                              {:target-vertices-fn (constantly (tie-bottom-vertices centre-vertex size))
                                               :duration           500} ; wait
                                              ]
                                   :repeat   true}})))

(defn- init-tie-texture []
  (let [gr (q/create-graphics c/canvas-width c/canvas-height)]
    (q/with-graphics gr
      (q/background 0 0 255 255)
      (q/fill 255)
      (q/with-rotation [(/ q/PI 6)]
        (doseq [y (range -900 900 10)]
          (q/rect 0 y 1500 3))))
    (reset! tie-texture gr)))

(defn- spawn-fire [spawn size]
  ;; flames
  (particles/add-emitter! 10
                          (* 5 size)
                          (* 4 size)
                          :shrink
                          {:interpolate {:start [252 (* 7 size) 3 25]
                                         :end   [252 0 3 0]}}
                          spawn
                          [0 (* -20 size)]
                          (* size 200)
                          [size (/ size 2.5)]
                          [(* 3 size) (* 2 size)]
                          (* size 40))
  ;; smoke
  (particles/add-emitter! 10
                          (* 5 size)
                          (* 4 size)
                          :shrink
                          {:interpolate {:start [30 30 30 25]
                                         :end   [10 10 10 175]}}
                          spawn
                          [0 (* -20 size)]
                          (* size 400)
                          [size (/ size 2.5)]
                          [(* 4 size) (* 2 size)]
                          (* size 40)))

(defn draw-creepy-gradient []
  (when (q/loaded? @gradient-shader)
    (q/shader @gradient-shader)
    (q/set-uniform @gradient-shader "u_resolution" (array c/canvas-width c/canvas-height))
    (q/set-uniform @gradient-shader "max_distance" (u/distance c/canvas-top-left [0 0]))
    (q/set-uniform @gradient-shader "min_distance" (+ 350 (* 50 (js/Math.sin (* 0.002 (q/millis))))))
    (q/blend-mode :add)
    (q/rect (x c/canvas-top-left)
            (y c/canvas-top-left)
            c/canvas-width
            c/canvas-height)
    (q/blend-mode :blend)
    (.resetShader (q/current-graphics))))

(defn spray-blood! [spawn]
  (particles/spray-particles! spawn
                              2000
                              5
                              :shrink
                              {:interpolate {:start [51 5 5 120]
                                             :end   [51 5 5 0]}}
                              (vector/set-length (q/random-2d) 250)
                              800
                              [10 10]
                              [100 100]
                              50))

(defn spray-blood-from-tie! []
  (spray-blood! [(q/random -10 10) (q/random -20 100)]))

(defn setup []
  (q/pixel-density 1) ; Necessary so that the shader works on mac screens.
  (init-tie-texture)
  (q/image @tie-texture 0 0) ; The tie texture doesn't render unless this is done and I have no idea why
  (q/background 0 0 0)
  (q/stroke 255 255 255)
  (vb/clear!)
  (init-tentacles)
  (reset! gradient-shader (q/load-shader "gradient.frag" "gradient.vert"))
  (vb/add-shape! (tie-bottom c/canvas-centre 2))
  (vb/add-shape! (tie-top c/canvas-centre 2))
  (spawn-fire (vector/add c/canvas-centre [4 110]) 4)
  (spawn-fire (vector/add c/canvas-centre [3.5 -25]) 3.2)
  (spawn-fire (vector/add c/canvas-centre [2 25]) 1.5)
  (spawn-fire (vector/add c/canvas-centre [-10 50]) 2))

(defn draw []
  (q/background 0 0 0)
  (draw-creepy-gradient))

(defn mouse-clicked []
  (spray-blood-from-tie!))