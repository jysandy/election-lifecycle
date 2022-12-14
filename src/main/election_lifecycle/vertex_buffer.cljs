(ns election-lifecycle.vertex-buffer
  (:require [clojure.walk :as walk]))

(comment
  ;; The vertex buffer contains a list of shapes which are to be rendered.
  ;; Shapes might look something like this:

  {:type      :line-list
   :vertices  []
   :animation {;; The current animation being played.
               :current {:target-vertices []
                         :start-time      50
                         :end-time        100
                         :step-index      1}
               ;; The next steps of the animation.
               :path    {:steps  [{:target-vertices-fn (fn [])
                                   :duration           300}
                                  {:target-vertices-fn (constantly [1 2 3 4])
                                   :duration           300}]
                         :repeat false ; setting repeat to true will make all steps repeat
                         }}
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

(defn- animation-step->current-animation [animation-step step-index current-time]
  (when animation-step
    {:target-vertices ((:target-vertices-fn animation-step))
     :start-time      current-time
     :end-time        (+ current-time (:duration animation-step))
     :step-index      step-index}))

(defn- finish-animation [{:keys [animation] :as shape}]
  (-> shape
      (assoc :vertices (:target-vertices (:current animation)))))

(defn- pickup-next-animation [{:keys [animation] :as shape} current-time]
  (let [{:keys [steps repeat]} (:path animation)
        next-step-index (mod (inc (:step-index (:current animation)))
                             (count steps))
        next-step       (nth steps next-step-index)]
    (if (and (zero? next-step-index)
             (not repeat))
      (-> shape
          (assoc animation nil))
      (-> shape
          (assoc-in [:animation :current] (animation-step->current-animation next-step
                                                                             next-step-index
                                                                             current-time))))))

(defn- current-animation-ended? [{:keys [animation]} current-time]
  (<= (:end-time (:current animation)) current-time))

(defn process-animations! [current-time]
  (swap! vertex-buffer
         update-shapes
         :animation
         (fn [{:keys [animation] :as shape}]
           (cond
             (not animation) shape

             (and (:current animation)
                  (current-animation-ended? shape current-time)
                  (nil? (:path animation))) (-> shape
                                                finish-animation
                                                (assoc :animation nil))

             (and (:current animation)
                  (current-animation-ended? shape current-time)
                  (some? (:path animation))) (-> shape
                                                 finish-animation
                                                 (pickup-next-animation current-time))

             (and (:current animation)
                  (not (current-animation-ended? shape current-time))
                  (some? (:path animation))) shape

             (and (not (:current animation))
                  (some? (:path animation))) (pickup-next-animation shape current-time)))))
