(ns election-lifecycle.drawing
  (:require [quil.core :as q]
            [election-lifecycle.animation :as animation]
            [election-lifecycle.particles :as particles]))

(def x first)
(def y second)

(defn connect-the-dots
  "Connects points by drawing line segments between them."
  [vertices]
  (doseq [[p1 p2] (partition 2 1 vertices)]
    (q/line p1 p2)))

(defn- screen-to-texture-space [[x y]]
  [(q/map-range x (- (/ (q/width) 2)) (/ (q/width) 2) 0 1)
   (q/map-range y (- (/ (q/height) 2)) (/ (q/height) 2) 0 1)])

(defn draw-vertex-list
  ([vertices]
   (draw-vertex-list vertices nil nil))
  ([vertices mode]
   (draw-vertex-list vertices mode nil))
  ([vertices mode texture]
   (if mode
     (q/begin-shape mode)
     (q/begin-shape))
   (when texture
     (q/texture texture))
   (doseq [[x y] vertices]
     (if texture
       (apply q/vertex x y (screen-to-texture-space [x y]))
       (q/vertex x y)))
   (q/end-shape :close)))

(defmulti draw-shape :type)

(defmethod draw-shape :line-list [{:keys [vertices animation stroke fill]}]
  (let [animated-vertices (if animation
                            (animation/animate-vertices vertices
                                                        (:target-vertices animation)
                                                        (:start-time animation)
                                                        (:end-time animation)
                                                        (q/millis))
                            vertices)]
    (if stroke
      (apply q/stroke stroke)
      (q/no-stroke))
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
    (if stroke
      (apply q/stroke stroke)
      (q/no-stroke))
    (when fill
      (apply q/fill fill))
    (draw-vertex-list animated-vertices nil texture)))

(defmethod draw-shape :quad-strip [{:keys [vertices animation stroke fill ]}]
  (let [animated-vertices (if animation
                            (animation/animate-vertices vertices
                                                        (:target-vertices animation)
                                                        (:start-time animation)
                                                        (:end-time animation)
                                                        (q/millis))
                            vertices)]
    (if stroke
      (apply q/stroke stroke)
      (q/no-stroke))
    (when fill
      (apply q/fill fill))
    ;; Vertices should be zig-zagged, not clockwise or counterclockwise.
    (doseq [[v1 v2 v3 v4] (partition 4 2 animated-vertices)]
      (q/begin-shape)
      (q/vertex (x v1) (y v1))
      (q/vertex (x v2) (y v2))
      (q/vertex (x v4) (y v4))
      (q/vertex (x v3) (y v3))
      (q/end-shape :close))))

(defn draw-particle [particle]
  (let [[x y] (particles/position particle (q/millis))
        radius (particles/radius particle (q/millis))]
    (q/no-stroke)
    (apply q/fill (particles/color particle (q/millis)))
    (q/blend-mode :add)
    (q/ellipse x y radius radius)))
