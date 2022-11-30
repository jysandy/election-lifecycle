(ns election-lifecycle.vector)

(def x first)
(def y second)

(defn add
  "Add two vectors."
  [v1 v2]
  [(+ (x v1) (x v2))
   (+ (y v1) (y v2))])

(defn scale
  "Multiply a vector v with a scalar n."
  [[x y] n]
  [(* x n)
   (* y n)])
