(ns election-lifecycle.drawing
  (:require [quil.core :as q]
            [election-lifecycle.animation :as animation]))

(defn connect-the-dots
  "Connects points by drawing line segments between them."
  [vertices]
  (doseq [[p1 p2] (partition 2 1 vertices)]
    (q/line p1 p2)))

(defn draw-polygon
  [vertices]
  (q/begin-shape)
  (doseq [[x y] vertices]
    (q/vertex x y))
  (q/end-shape))

(defmulti draw-shape :type)

(defmethod draw-shape :line-list [{:keys [vertices animation stroke fill]}]
  (let [animated-vertices (if animation
                            (animation/animate-vertices vertices
                                                        (:target-vertices animation)
                                                        (:start-time animation)
                                                        (:end-time animation)
                                                        (q/millis))
                            vertices)]
    (apply q/stroke stroke)
    (apply q/fill fill)
    (connect-the-dots animated-vertices)))

(defmethod draw-shape :polygon [{:keys [vertices animation stroke fill]}]
  (let [animated-vertices (if animation
                            (animation/animate-vertices vertices
                                                        (:target-vertices animation)
                                                        (:start-time animation)
                                                        (:end-time animation)
                                                        (q/millis))
                            vertices)]
    (when stroke
      (apply q/stroke stroke))
    (when fill
      (apply q/fill fill))
    (draw-polygon animated-vertices)))
