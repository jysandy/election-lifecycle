(ns election-lifecycle.core
  (:require [quil.core :as q]
            [tranquil.drawing :as drawing]
            [tranquil.vertex-buffer :as vb]
            [election-lifecycle.sketch :as sketch]
            [tranquil.particles :as particles]
            [election-lifecycle.config :as c])
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
  (vb/process-animations! (q/millis))
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
                       :mouse-clicked sketch/mouse-clicked
                       :size [c/canvas-width c/canvas-height])))
