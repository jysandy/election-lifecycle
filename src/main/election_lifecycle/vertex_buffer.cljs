(ns election-lifecycle.vertex-buffer
  (:require [clojure.walk :as walk]))

(comment
  ;; The vertex buffer contains a list of shapes which are to be rendered.
  ;; Shapes might look something like this:

  {:type      :line-list
   :vertices  []
   :animation {:target-vertices []
               :start-time      50
               :end-time        100}
   :meta      {:category :tentacle}}

  ;; One can add fields for stroke colour, fill colour and so on as well.

  ;; An animator function will compute the final positions of each shape and render them.
  ;; A second function will garbage-collect finished animations.
  )

(defonce vertex-buffer (atom []))

(defn clear! []
  (reset! vertex-buffer []))

(defn add-shape! [shape]
  (swap! vertex-buffer conj shape))

(defn update-shapes [shapes filter-fn update-fn]
  (walk/postwalk
    (fn [shape]
      (if (and (:type shape)
               (:vertices shape)
               (filter-fn shape))
        (update-fn shape)
        shape))
    shapes))

(defn update-by-category [shapes category update-fn]
  (update-shapes shapes
                 #(= category (get-in % [:meta :category]))
                 update-fn))

(defn cleanup-finished-animations! [current-time]
  (swap! vertex-buffer
         update-shapes
         :animation
         (fn [{:keys [animation] :as shape}]
           (if (<= (:end-time animation) current-time)
             (assoc shape
               :vertices (:target-vertices animation)
               :animation nil)
             shape))))
