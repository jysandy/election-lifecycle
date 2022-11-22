(ns election-lifecycle.animation
  (:require [quil.core :as q]))

(def x first)
(def y second)

(defn animate-vertex
  [start end start-time end-time current-time]
  (cond
    (<= current-time start-time) start
    (<= end-time current-time) end
    :else
    [(q/lerp (x start) (x end) (/ (- current-time start-time)
                                  (- end-time start-time)))
     (q/lerp (y start) (y end) (/ (- current-time start-time)
                                  (- end-time start-time)))]))

(defn animate-line-list
  [start-vertices target-vertices start-time end-time current-time]
  (map (fn [start-vertex end-vertex]
         (animate-vertex start-vertex
                         end-vertex
                         start-time
                         end-time
                         current-time))
       start-vertices
       target-vertices))
