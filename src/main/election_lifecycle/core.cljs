(ns election-lifecycle.core
  (:require [quil.core :as q]
            [election-lifecycle.drawing :as drawing]
            [election-lifecycle.vertex-buffer :as vb]
            [election-lifecycle.sketch :as sketch]
            [election-lifecycle.particles :as particles])
  (:require-macros [quil.core]))

(defonce font (atom nil))

(defn draw []
  (q/clear)
  (sketch/draw)
  (q/blend-mode :blend)
  (doseq [shape @vb/vertex-buffer]
    (drawing/draw-shape shape))
  (doseq [particle @particles/particle-buffer]
    (drawing/draw-particle particle))
  (vb/cleanup-finished-animations! (q/millis))
  (particles/garbage-collect-particles! (q/millis))
  (q/stroke 255 255 255 255)
  (q/fill 255 255 255 255)
  (q/text-font @font)
  (q/text (Math/round (q/current-frame-rate)) -590 -430))

(def the-sketch (atom nil))

(defn setup []
  (reset! font (q/load-font "Roboto-Regular.ttf"))
  (q/frame-rate 60)
  (q/ortho)
  (sketch/setup))

(defn init []
  (reset! the-sketch (q/sketch
                       :renderer :p3d
                       :host "canvas-id"
                       :setup setup
                       :draw draw
                       :size [sketch/canvas-width sketch/canvas-height])))
