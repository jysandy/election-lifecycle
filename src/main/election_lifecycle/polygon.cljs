(ns election-lifecycle.polygon
  (:require [election-lifecycle.vector :as vector]))

(defn isosceles-triangle [base-center base-width tip-direction height]
  (let [base-to-tip   (-> (vector/sub tip-direction base-center)
                          (vector/set-length height))
        base-vertices (->> (vector/normals base-to-tip)
                           (map #(vector/scale % (/ base-width 2)))
                           (map (partial vector/add base-center)))
        tip-vertex    (vector/add base-center base-to-tip)]
    (conj (vec base-vertices) tip-vertex)))

(defn make-polygon [vertex-list]
  {:type      :polygon
   :vertices  vertex-list
   :animation nil
   :meta      {}})
