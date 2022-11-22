(ns election-lifecycle.drawing
  (:require [quil.core :as q]
            [election-lifecycle.animation :as animation]))

(defn connect-the-dots
  "Connects points by drawing line segments between them."
  [vertices]
  (doseq [[p1 p2] (partition 2 1 vertices)]
    (q/line p1 p2)))

(defmulti draw-shape :type)

(defmethod draw-shape :line-list [{:keys [vertices animation]}]
  (let [animated-vertices (if animation
                            (animation/animate-line-list vertices (:target-vertices animation)
                                                         (:start-time animation)
                                                         (:end-time animation)
                                                         (q/millis))
                            vertices)]
    (connect-the-dots animated-vertices)))
