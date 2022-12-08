(ns election-lifecycle.vector
  (:require [quil.core :as q]
            [election-lifecycle.utils :as u]))

(def x first)
(def y second)

(defn add
  "Add two vectors."
  ([v1 v2]
   [(+ (x v1) (x v2))
    (+ (y v1) (y v2))])
  ([v1 v2 & vs]
   (reduce add (add v1 v2) vs)))

(defn scale
  "Multiply a vector v with a scalar n."
  [[x y] n]
  [(* x n)
   (* y n)])

(defn sub
  ([v1 v2]
   (add v1 (scale v2 -1)))
  ([v1 v2 & vs]
   (reduce sub (sub v1 v2) vs)))

(defn norm [v]
  (scale v (/ 1 (apply q/mag v))))

(defn set-length [v l]
  (scale (norm v) l))

(defn normals
  "Returns the two unit vectors perpendicular to the given vector."
  [[x y]]
  (let [v1 (if (zero? x)
             [1 0]
             (norm [(/ (- y) x), 1]))]
    [v1 (scale v1 -1)]))

(defn dot
  "Dot product or scalar product of two vectors."
  [v1 v2]
  (reduce + (map * v1 v2)))

(defn cross-magnitude
  [v1 v2]
  (- (* (x v1) (y v2)) (* (x v2) (y v1))))

(defn is-right?
  "Is v1 to the right of v2? Assumes that v1 and v2 both start from the origin.
  Also assumes a left-handed coordinate system."
  [v1 v2]
  (> (cross-magnitude v1 v2) 0))

(defn sort-right-to-left [reference-vector vs]
  (sort
    (fn [v1 v2]
      (< (cross-magnitude v1 reference-vector)
         (cross-magnitude v2 reference-vector)))
    vs))

(defn sort-left-to-right [reference-vector vs]
  (reverse (sort-right-to-left reference-vector vs)))

(defn directional-jitter
  "Jitters the vector v horizontally along the horizontal-unit-vector,
  and vertically perpendicular to the horizontal vector."
  [v jitter-factors horizontal-unit-vector]
  (let [vertical-unit-vector (first (normals horizontal-unit-vector))]
    (add v
         (scale horizontal-unit-vector (u/gaussian-jitter 0 (x jitter-factors)))
         (scale vertical-unit-vector (u/gaussian-jitter 0 (y jitter-factors))))))