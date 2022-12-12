(ns election-lifecycle.utils
  (:require [quil.core :as q]
            [election-lifecycle.config :as c]))

(def x first)
(def y second)

(defn distance [p1 p2]
  (Math/sqrt (+ (-> (- (x p2) (x p1))
                    (Math/pow 2))
                (-> (- (y p2) (y p1))
                    (Math/pow 2)))))

(defn gaussian-jitter
  [n jitter-factor]
  (+ n (* jitter-factor (q/random-gaussian))))

(defn gaussian-jitter-seq
  [jitter-factor]
  (lazy-seq
    (cons (* jitter-factor (q/random-gaussian)) (gaussian-jitter-seq jitter-factor))))

(defn gaussian-jitter-vector
  [v jitter-factors]
  [(gaussian-jitter (x v) (x jitter-factors))
   (gaussian-jitter (y v) (y jitter-factors))])

(defn screen-to-world
  "Converts on-screen pixel coordinates to the 3D coordinate system."
  [vertex canvas-width canvas-height]
  [(- (x vertex) (/ canvas-width 2))
   (- (y vertex) (/ canvas-height 2))])

(defn world-to-screen
  [vertex canvas-width canvas-height]
  [(+ (x vertex) (/ canvas-width 2))
   (+ (y vertex) (/ canvas-height 2))])

(defn mouse-position []
  (screen-to-world [(q/mouse-x) (q/mouse-y)] c/canvas-width c/canvas-height))

(defn out-of-bounds?
  [vertex]
  (not (and (<= (- (/ c/canvas-width 2)) (x vertex) (/ c/canvas-width 2))
            (<= (- (/ c/canvas-height 2)) (y vertex) (/ c/canvas-height 2)))))

