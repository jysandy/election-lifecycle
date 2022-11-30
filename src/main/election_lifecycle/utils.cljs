(ns election-lifecycle.utils)

(def x first)
(def y second)

(defn distance [p1 p2]
  (Math/sqrt (+ (-> (- (x p2) (x p1))
                    (Math/pow 2))
                (-> (- (y p2) (y p1))
                    (Math/pow 2)))))

(defn screen-to-world
  "Converts on-screen pixel coordinates to the 3D coordinate system."
  [vertex canvas-width canvas-height]
  [(- (x vertex) (/ canvas-width 2))
   (- (y vertex) (/ canvas-height 2))])