(ns election-lifecycle.polygon)

(defn make-polygon [vertex-list]
  {:type      :polygon
   :vertices  vertex-list
   :animation nil
   :meta      {}})
