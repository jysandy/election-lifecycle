(ns election-lifecycle.core
  (:require [reagent.dom :as rdom]
            [quil.core :as q]
            [election-lifecycle.drawing :as drawing]
            [election-lifecycle.vertex-buffer :as vb]
            [election-lifecycle.sketch :as sketch])
  (:require-macros [quil.core]))

(defn some-component []
  [:div
   [:h3 "Election Reddy's tragic past"]])

(defn draw []
  (sketch/draw)
  (doseq [shape @vb/vertex-buffer]
    (drawing/draw-shape shape))
  (vb/cleanup-finished-animations! (q/millis))
  (q/fill 255 255 255 255)
  (q/text (Math/round (q/current-frame-rate)) 10 20))

(def the-sketch (atom nil))

(defn init []
  (reset! the-sketch (q/sketch
                       :renderer :p2d
                       :host "canvas-id"
                       :setup sketch/setup
                       :draw draw
                       :size [sketch/canvas-width sketch/canvas-height]))
  (rdom/render [some-component]
               (.getElementById js/document "app")))
