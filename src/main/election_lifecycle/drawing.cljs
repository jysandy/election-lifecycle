(ns election-lifecycle.drawing
  (:require [quil.core :as q]
            [election-lifecycle.animation :as animation]))

(defn connect-the-dots
  "Connects points by drawing line segments between them."
  [vertices]
  (doseq [[p1 p2] (partition 2 1 vertices)]
    (q/line p1 p2)))

(defn- screen-to-texture-space [[x y]]
  [(q/map-range x (- (/ (q/width) 2)) (/ (q/width) 2) 0 1)
   (q/map-range y (- (/ (q/height) 2)) (/ (q/height) 2) 0 1)])

(defn draw-polygon
  ([vertices]
   (draw-polygon vertices nil))
  ([vertices texture]
   (q/begin-shape)
   (when texture
     (q/texture texture))
   (doseq [[x y] vertices]
     (apply q/vertex x y (screen-to-texture-space [x y])))
   (q/end-shape)))

(defmulti draw-shape :type)

(defmethod draw-shape :line-list [{:keys [vertices animation stroke fill]}]
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
    (connect-the-dots animated-vertices)))

(defmethod draw-shape :polygon [{:keys [vertices animation stroke fill texture]}]
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
    (draw-polygon animated-vertices texture)))
