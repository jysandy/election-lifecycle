(ns election-lifecycle.tentacle
  (:require [tranquil.utils :as u]
            [election-lifecycle.config :as c]
            [quil.core :as q]
            [tranquil.line :as line]
            [tranquil.vector :as vector]))

(defn- distort-spine
  [line-points jitter-factors horizontal]
  (map #(vector/directional-jitter % jitter-factors horizontal) line-points))

(defn- split-angle
  "Returns a vector that divides the angle between the two given vectors in half."
  [v1 v2]
  (if (zero? (vector/cross-magnitude v1
                                     v2))
    (-> (vector/normals v2)
        first)
    (vector/sub v1 v2)))

(defn generate-tentacle-vertices
  [base-vertex end-vertex tentacle-length]
  (let [n-segments              12
        tip-vertex              (-> (vector/sub end-vertex base-vertex)
                                    (vector/set-length tentacle-length)
                                    (vector/add base-vertex))
        base-to-tip             (vector/sub tip-vertex base-vertex)
        [centre-to-base1 centre-to-base2] (->> (vector/normals base-to-tip)
                                               (map #(vector/set-length % (/ c/tentacle-width 2)))
                                               (vector/sort-left-to-right base-to-tip))
        tentacle-spine-vertices (distort-spine (line/divide-line [base-vertex tip-vertex]
                                                                 n-segments)
                                               [5 0.1]
                                               (vector/norm centre-to-base2))
        left-base               (vector/add base-vertex centre-to-base1)
        right-base              (vector/add base-vertex centre-to-base2)
        body-vertices           (->> (partition 3 1 tentacle-spine-vertices)
                                     (map-indexed (fn [i [a b c]]
                                                    (let [rib-width      (q/map-range (- n-segments i 1) 0 n-segments 0 c/tentacle-width)
                                                          spine-to-edge1 (vector/set-length (split-angle (vector/sub c b)
                                                                                                         (vector/sub b a))
                                                                                            (/ rib-width 2))
                                                          spine-to-edge2 (vector/scale spine-to-edge1 -1)
                                                          [spine-to-left-edge-vertex
                                                           spine-to-right-edge-vertex] (vector/sort-left-to-right
                                                                                         base-to-tip
                                                                                         [spine-to-edge1 spine-to-edge2])]
                                                      [(vector/add b spine-to-left-edge-vertex)
                                                       (vector/add b spine-to-right-edge-vertex)])))
                                     (apply concat))]
    (doall (concat [left-base right-base] body-vertices [tip-vertex tip-vertex]))))

(defn- tentacle-end-position
  [mouse-position]
  (if (u/out-of-bounds? mouse-position)
    c/canvas-centre
    mouse-position))

(defn tentacle-length
  [start-position end-position]
  (min (+ 150 (rand-int 100))
       (- (u/distance start-position end-position) 50)))

(defn generate-tentacle
  [base-vertex]
  {:type      :quad-strip
   :vertices  (generate-tentacle-vertices base-vertex
                                          c/canvas-centre
                                          (tentacle-length base-vertex
                                                           (tentacle-end-position c/canvas-centre)))
   :animation {:current nil
               :path    {:steps  [{:target-vertices-fn (fn []
                                                         (generate-tentacle-vertices
                                                           base-vertex
                                                           (tentacle-end-position (u/mouse-position))
                                                           (tentacle-length base-vertex
                                                                            (tentacle-end-position (u/mouse-position)))))
                                   :duration           300}]
                         :repeat true}}
   :stroke    nil
   :fill      [0 0 0 180]
   :meta      {:category :tentacle}})

